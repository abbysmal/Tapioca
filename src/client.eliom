{shared{
let (>>=) = Lwt.bind

open Eliom_content
open Html5.D

type diff = (int  * string) array
  deriving(Json)

type request = {from_revision : int; diffs : (int * string) list}
    deriving(Json)
}}

{server{

let content =
  Html5.F.(
    div ~a:[a_contenteditable `True; a_id "editor"]
      [span []])


let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt : [`Applied of int | `Refused] Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<request>)
    ()

}}

{client{

module Html = Dom_html

let (>>=) = Lwt.bind
let ( |> ) x f = f x

open Dom

let load_document editor old =
  Eliom_client.call_ocaml_service ~service:%Services.get_document "toto" ()
  >>= fun response ->
  begin
    match response with
    | `Result document -> editor##innerHTML <- (Js.string document);
      old := editor##innerHTML; Lwt.return_unit
    | `NotConnected -> Lwt.return_unit
  end

let onload _ =
  Random.self_init ();
  let oldContent = ref (Js.string "") in
  let rev = ref 0 in
  let d = Html.document in
  let self_id = Random.int 4096 in

  let body =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let editor = Html.createDiv d in

  editor##style##border <- Js.string "2px #c5cd5c solid";
  editor##id  <- Js.string "editorFrame";
  Dom.appendChild body editor;
  ignore(load_document editor oldContent);

  Lwt_js_events.(
    async
      (fun () ->
        inputs Dom_html.document
           (fun ev _ ->
             let dmp = DiffMatchPatch.make () in
             let diff = DiffMatchPatch.diff_main dmp (Js.to_string (!oldContent))
                 (Js.to_string (editor##innerHTML)) in
             let req = { from_revision = !rev; diffs = (Array.to_list diff); } in
             Eliom_client.call_ocaml_service ~service:%send_patch () req
             >>= fun response ->
             begin
               match response with
               | `Applied _ -> rev := !rev + 1; oldContent := (editor##innerHTML); Lwt.return_unit
               | `Refused -> Lwt.return_unit
             end
  )))

  (* Lwt.async (fun () -> Lwt_stream.iter *)
  (* (fun (id, diff) -> *)
  (*   if id != self_id then *)
  (*     begin *)
  (*       Array.iter (fun (i, s) -> Eliom_lib.debug "op: %d str %s\n" i s) diff; *)
  (*       let dmp = DiffMatchPatch.make () in *)
  (*       let patch = DiffMatchPatch.patch_make dmp *)
  (*           (Js.to_string editor##innerHTML) diff in *)
  (*       editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply dmp patch *)
  (*           (Js.to_string editor##innerHTML); *)
  (*       oldContent := editor##innerHTML *)
  (*     end *)
  (*   else *)
  (*     () *)
  (* ) *)
  (* (Eliom_bus.stream %bus)) *)

let _ = Eliom_client.onload @@ fun () -> onload ()

}}

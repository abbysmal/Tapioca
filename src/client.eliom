{shared{
let (>>=) = Lwt.bind

open Eliom_content
open Html5.D

type diff = (int  * string) array
    deriving(Json)

type request = {from_revision : int; diffs : (int * string) list}
    deriving(Json)

let print_state rev text =
  Eliom_lib.debug "id: %d\n%s\nEND TEXT\n" rev text

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

let make_diff text old_text rev =
  let dmp = DiffMatchPatch.make () in
  let diff = DiffMatchPatch.diff_main dmp old_text text in
  {from_revision = rev; diffs = (Array.to_list diff);}

let onload _ =
  Random.self_init ();

  (* Is the current revision server-side *)
  let shadow_copy = ref (Js.string "") in
  (* Is the revision number of this client *)
  let rev = ref 0 in
  (* this client id *)
  let client_id = Random.int 4096 in

  let d = Html.document in

  let body =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let editor = Html.createDiv d in

  editor##style##border <- Js.string "2px #c5cd5c solid";
  editor##id  <- Js.string "editorFrame";
  Dom.appendChild body editor;
  (* get document content *)
  ignore(load_document editor shadow_copy);

  (* changes handler *)
  Lwt_js_events.(
    async
      (fun () ->
        inputs Dom_html.document
           (fun ev _ ->
             Lwt_js.sleep 1. >>= fun () ->
             let diff = make_diff (Js.to_string editor##innerHTML)
                 (Js.to_string !shadow_copy) !rev in
             Eliom_client.call_ocaml_service ~service:%send_patch () diff
             >>= fun response ->
             begin
               match response with
               | `Applied srev, scopy -> Lwt.return @@ rev := srev; shadow_copy := scopy
               | `Refused -> Lwt.return ()
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

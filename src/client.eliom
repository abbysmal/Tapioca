{shared{

let (>>=) = Lwt.bind

open Eliom_content
open Html5.D

type diff = (int  * string) array
    deriving(Json)

type request = {client : int; from_revision : int; diffs : (int * string) list}
    deriving(Json)

let print_state rev text =
  Eliom_lib.debug "id: %d\n%s\nEND TEXT\n" rev text

type bus_message =
  | Patch of (int * diff * int)
  | Hello

}}

{server{

let content =
  Html5.F.(
    div ~a:[a_contenteditable `True; a_id "editor"]
      [span []])

let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt : [`Applied of int * string | `Refused of int * string] Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<request>)
    ()

let patches_bus = Eliom_bus.create
    ~scope:Eliom_common.site_scope Json.t<int * diff * int>

}}

{client{

module Html = Dom_html

let (>>=) = Lwt.bind
let ( |> ) x f = f x

open Dom

let load_document editor old rev =
  Eliom_client.call_ocaml_service ~service:%Services.get_document "toto" ()
  >>= fun response ->
  begin
    match response with
    | `Result (document, id) -> editor##innerHTML <- (Js.string document);
      old := editor##innerHTML; rev := id; Lwt.return_unit
    | `NotConnected -> Lwt.return_unit
  end

let make_diff text old_text rev client_id =
  let dmp = DiffMatchPatch.make () in
  let diff = DiffMatchPatch.diff_main dmp old_text text in
  {from_revision = rev; diffs = (Array.to_list diff); client = client_id;}

let get_editor _ = Js.Opt.get (Html.document##getElementById (Js.string "editor")) (fun () -> assert false)

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

  Dom.appendChild body editor;
  (* get document content *)

  (* changes handler *)
  Lwt_js_events.(
    async
    (fun () ->
        inputs Dom_html.document
           (fun ev _ ->
             Lwt_js.sleep 0.3 >>= fun () ->
             let editor = get_editor () in
             let diff = make_diff (Js.to_string editor##innerHTML)
                 (Js.to_string !shadow_copy) !rev client_id in
             Eliom_client.call_ocaml_service ~service:%send_patch () diff
             >>= fun response ->
             begin
               match response with
               | `Applied (srev, scopy) -> rev := srev;
                 shadow_copy := (Js.string scopy); Lwt.return_unit
               | `Refused (srev, scopy) -> shadow_copy := (Js.string scopy); Lwt.return ()
             end
  )));
  ignore(load_document editor shadow_copy rev);

  Lwt.async (fun () -> Lwt_stream.iter
  (fun (id, diff, prev) ->
    if id != client_id then
      begin
        let editor = get_editor () in
        let dmp = DiffMatchPatch.make () in
        let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
        let patch_editor = DiffMatchPatch.patch_make dmp (Js.to_string editor##innerHTML) diff in
        editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor (Js.to_string editor##innerHTML);
        shadow_copy := Js.string @@ DiffMatchPatch.patch_apply dmp patch_scopy (Js.to_string !shadow_copy);
        rev := prev;
      end
    else
      print_endline "patch de moi";
  )
  (Eliom_bus.stream %patches_bus))

let _ = Eliom_client.onload @@ fun () -> onload ()

}}

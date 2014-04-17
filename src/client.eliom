{shared{
let (>>=) = Lwt.bind

open Eliom_content
open Html5.D

type diff = (int  * string) array
  deriving(Json)
}}

let save_data data =
  let store = Ocsipersist.open_store "toto" in
  Ocsipersist.make_persistent ~store ~name:"toto" ~default:"defautl"
  >>= fun v ->
  Ocsipersist.get v
  >>= fun value ->
  begin
    match data with
    | Edition.DeleteChar -> Ocsipersist.set v @@ Str.string_before value (String.length value -1)
    | Edition.WriteChar str -> Eliom_lib.debug "%s" value; Ocsipersist.set v @@ value ^ str
  end

let save = server_function Json.t<Edition.operation> save_data

let bus = Eliom_bus.create Json.t<((int * string) array)>

{client{

module Html = Dom_html

let (>>=) = Lwt.bind
let ( |> ) x f = f x

open Dom

let load_document editor =
  Eliom_client.call_ocaml_service ~service:%Services.get_document "toto" ()
  >>= fun response ->
  begin
    match response with
    | `Result document -> Eliom_lib.debug "lol %s" document;Lwt.return
    (editor##innerHTML <-(Js.string document))
    | `NotConnected -> Lwt.return_unit
  end

let onload _ =
  let d = Html.document in
  let oldContent = ref (Js.string "") in

  let body =
    Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let editor = Html.createDiv d in

  editor##style##border <- Js.string "2px #c5cd5c solid";
  editor##id  <- Js.string "editorFrame";
  Dom.appendChild body editor;

  ignore(load_document editor);

  Lwt_js_events.(
    async
      (fun () ->
        inputs Dom_html.document
           (fun ev _ ->
             let dmp = DiffMatchPatch.make () in
             let diff = DiffMatchPatch.diff_main dmp (Js.to_string
             (!oldContent)) (Js.to_string (editor##innerHTML)) in
             Eliom_bus.write %bus (diff : diff);
             oldContent := (editor##innerHTML);
             Lwt.return_unit
           )))


let _ = Eliom_client.onload @@ fun () -> onload ()

}}

{server{

let content =
  Html5.F.(
    div ~a:[a_contenteditable `True; a_id "editor"]
    [span []])
}}

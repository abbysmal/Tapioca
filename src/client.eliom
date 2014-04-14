{shared{
let (>>=) = Lwt.bind
open Eliom_content
open Html5.D
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


{client{

module Html = Dom_html

let (>>=) = Lwt.bind
let ( |> ) x f = f x

open Dom

let rec send_data changes =
  Lwt_js.sleep 1.0
  >>= fun () ->
  let send_patch value = ignore @@ %save value in
  let to_send = !changes in
  changes := [];
  List.iter send_patch to_send;
  send_data changes


let load_document editor =
  Eliom_client.call_ocaml_service ~service:%Services.get_document "toto" ()
  >>= fun response ->
  begin
    match response with
    | `Result document -> Eliom_lib.debug "lol %s" document;Lwt.return
    (editor##innerHTML <-(Js.string document))
    | `NotConnected -> Lwt.return_unit
  end

let add_backslash _ = %save (Edition.WriteChar("</br></br>"))
let add character = %save (Edition.WriteChar(Js.to_string character))
let delete_one_char _ = %save (Edition.DeleteChar)

let onload _ =
  let d = Html.document in

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
         keypresses Dom_html.document
           (fun ev _ ->
              let key = (Js.Optdef.get (ev##charCode) (fun() -> 0)) in
              if key = 0 then
                begin
                  match ev##keyCode with
                  | 13 -> add_backslash (); Lwt.return_unit
                  | 8 -> delete_one_char (); Lwt.return_unit
                  | keypressed -> Eliom_lib.debug "%i" keypressed; Lwt.return_unit
                end
              else
                begin
                  let ch = Js.string_constr##fromCharCode (key) in
                  add ch;
                  Lwt.return_unit
                end
           )))


let _ = Eliom_client.onload @@ fun () -> onload ()

}}

{server{

let content =
  Html5.F.(
  Ew_editable.editable_name
    ~a:([a_class ["editable"]])
    ~edit:(span [pcdata "edit"])
    ~confirm:(span [pcdata "confirm"])
    ~cancel:(span [pcdata "cancel"])
    ~default_name:("editor")
    ~content:(pcdata "content")
    ~callback:{string -> unit Lwt.t{(fun _ -> Lwt.return_unit)}})
}}

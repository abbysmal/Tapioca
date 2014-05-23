let (>>=) = Lwt.bind
include Edition
module Tapioca_app =
  Eliom_registration.App (
  struct
    let application_name = "tapioca"
  end)

let get_document_function name =
  Edition.get_shadowcopies ()
  >>= fun scopies ->
  match scopies with
  | [] -> Lwt.return `NotConnected
  | {id = id; text = scopy}::xs -> Lwt.return (`Result (scopy, id))

let () =
  Eliom_registration.Ocaml.register
    ~service:Client.send_patch
    (fun () patch ->
       Edition.handle_patch_request patch);

  Eliom_registration.Ocaml.register
    ~service:Services.get_document
    (fun name () -> get_document_function name);

  Tapioca_app.register
    ~service:Services.main_service
    (fun () () ->
       Lwt.return @@ Templates.format_page Client.content)

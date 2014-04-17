open Eliom_content
open Html5.D
open Html5.F

let (>>=) = Lwt.bind

module Tapioca_app =
  Eliom_registration.App (
  struct
    let application_name = "tapioca"
  end)

let get_document_function name =
  let store = Ocsipersist.open_store "toto" in
  Ocsipersist.make_persistent ~store ~name ~default:"default"
  >>= fun v ->
  Ocsipersist.get v

let () =
  Eliom_registration.Ocaml.register
    ~service:Services.get_document
    (fun name () -> get_document_function name
      >>= fun document ->
      Lwt.return @@ `Result document);

  Tapioca_app.register
    ~service:Services.main_service
    (fun () () ->
       Lwt.return
         (Eliom_tools.F.html
            ~title:"tapioca"
            ~css:[["css";"tapioca.css"]]
            ~js:[["js";"libs.js"]]
            (body [Client.content])))

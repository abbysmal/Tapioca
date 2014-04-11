{shared{
  open Eliom_content
  open Html5.D
}}

module Tapioca_app =
  Eliom_registration.App (
    struct
      let application_name = "tapioca"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Tapioca_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"tapioca"
           ~css:[["css";"tapioca.css"]]
           Html5.F.(body [
             h2 [""];
           ])))

let () =

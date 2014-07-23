let (>>=) = Lwt.bind
include Edition
module Tapioca_app =
  Eliom_registration.App (
  struct
    let application_name = "tapioca"
  end)

let () =
  Tapioca_app.register
    ~service:Services.main_service
    (fun () () ->
       Lwt.return @@ Templates.format_page Client.content)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let get_document =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Result of (string * int) | `NotConnected] Eliom_service.rt)
    ~get_params: (Eliom_parameter.unit)
    ()

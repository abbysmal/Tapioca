module type Null = sig end

let _ = (module Client : Null)
let _ = (module Editor : Null)

let _link = ()

let (>>=) = Lwt.bind

type response =
  | Applied of int
  | Rejected of (int * string) array list

type request = {from_revision : int; diffs : (int * string) list}

type revision = {id : int; text : string }

let (append_shadowcopies, get_shadowcopies) =
  let default_value = [{id = 0; text = ""}] in
  let eref = Eliom_reference.eref ~scope:Eliom_common.site_scope default_value in
  let get = Eliom_reference.get in
  ((fun elm -> get eref
    >>= fun shdwcopies -> Eliom_reference.set eref (elm::shdwcopies)),
  (fun () -> get eref))

let handle_patch_request request =
  let verify_patch cscopy oscopies =
    let cid, ctext = cscopy.id, cscopy.text in
    let rid, rdiffs = request.from_revision, request.diffs in
    match Patches.apply_diffs ctext request.diffs with
    | Patches.Failure _ -> Lwt.return (Rejected [])
    | Patches.Success ntext -> if rid = cid then Lwt.return (Applied (cid + 1))
      else Lwt.return (Rejected [])
  in
  get_shadowcopies ()
  >>= fun scopies ->
  match scopies with
  | [] -> Lwt.return (Rejected [])
  | x::xs -> verify_patch x xs

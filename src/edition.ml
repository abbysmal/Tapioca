let (>>=) = Lwt.bind

type response =
  | Applied of int
  | Rejected of (int * string) array list

type revision = {id : int; text : string }

let (append_shadowcopies, get_shadowcopies) =
  let default_value = [{id = 0; text = "default"}] in
  let eref = Eliom_reference.eref ~scope:Eliom_common.site_scope default_value in
  let get = Eliom_reference.get in
  ((fun elm -> get eref
     >>= fun shdwcopies -> Eliom_reference.set eref (elm::shdwcopies)),
  (fun () -> get eref))

let handle_patch_request (request : Client.request) =
  let verify_patch cscopy oscopies =
    let cid, ctext = cscopy.id, cscopy.text in
    Client.print_state cid ctext;
    let rid, rdiffs = request.from_revision, request.diffs in
    match Patches.apply_diffs ctext request.diffs with
    | Patches.Failure s -> Eliom_lib.debug "%s\n" s; Lwt.return (`Refused)
    | Patches.Success ntext -> if rid = cid then
        begin
          let ncopy = { id = cid + 1;
                        text = ntext; } in
          append_shadowcopies ncopy;
          Eliom_lib.debug "success"; Lwt.return (`Applied (cid + 1))
        end
      else begin Eliom_lib.debug "Id mismatch\n";Lwt.return (`Refused) end
  in
  get_shadowcopies ()
  >>= fun scopies ->
  match scopies with
  | [] -> Eliom_lib.debug "echeeec\n"; Lwt.return (`Refused)
  | x::xs -> verify_patch x xs

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
    let rid, rdiffs, ruid = request.from_revision, request.diffs, request.client in
    match Patches.apply_diffs ctext request.diffs with
    | Patches.Failure s -> print_endline s;Lwt.return (`Refused (cid, ctext))
    | Patches.Success ntext -> if rid = cid then
        begin
          let ncopy = { id = cid + 1;
                        text = ntext; } in
          append_shadowcopies ncopy;
          Eliom_bus.write Client.patches_bus (ruid, (Array.of_list rdiffs), cid);
          Lwt.return (`Applied (cid + 1, ntext))
        end
      else begin print_endline "cid != rid";Lwt.return (`Refused (cid, ctext)) end
  in
  get_shadowcopies ()
  >>= fun scopies ->
  match scopies with
  | [] -> Lwt.return (`Refused (0, ""))
  | x::xs -> verify_patch x xs

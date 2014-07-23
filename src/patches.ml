open Eliom_lib.Lwt_ops

type request = {client : int; from_revision : int; diffs : (int * string) list}
    deriving(Json)

type diff = (int  * string) array
    deriving(Json)

type bus_message =
  | Patch of (int * diff * int)
  | Hello of int
        deriving(Json)

type patch_result =
  | Failure of string
  | Success of string

type response =
  | Applied of int
  | Rejected of (int * string) array list

type doc = {id : int; text : string }

let new_document text = {id = 0; text = text}

let insert_at text str id =
  try
    begin
      let beginning = Str.string_before text id in
      let ending = Str.string_after text id in
      Success (beginning ^ str ^ ending)
    end
  with
  | _ -> Failure "Error while inserting text"


let apply_deletion text str i =
  try
    begin
      let current_chunk = Str.string_after !text i in
      let to_delete = Str.string_before current_chunk (String.length str) in
      if to_delete = str then
        begin
          text := (Str.string_before !text i) ^ (Str.string_after !text (i + (String.length str)));
          true
        end
      else
        false
    end
  with
  | Invalid_argument _ -> false


let apply_addition text str i =
  match insert_at !text str i with
  | Success str -> text := str; true
  | Failure _ -> false


let check_coherence i s text =
  try
    let start = Str.string_before text (i + (String.length s)) in
    let chunk = Str.string_after start i in
    chunk = s
  with
  | Invalid_argument _ -> false


let apply_diffs text diffs =
  let rtext = ref text in
  let rec inner diffs i =
    match diffs with
    | [] -> Success !rtext
    | (-1, str)::xs ->
      if apply_deletion rtext str i then
        inner xs i
      else
          Failure "Impossible to delete chunk"
    | (0, str)::xs -> if check_coherence i str !rtext then inner xs (i + (String.length str))
      else Failure "Retain don't match"
    | (1, str)::xs ->
      if apply_addition rtext str i then
        inner xs (i + (String.length str))
      else
        Failure "Impossible to patch chunk"
    | _ -> Failure "Unknown patch operation"
  in inner diffs 0



let handle_patch_request get_copy append_copy bus (request : request) =
  let verify_patch cscopy =
    let cid, ctext = cscopy.id, cscopy.text in
    let rid, rdiffs, ruid = request.from_revision, request.diffs, request.client in
    match apply_diffs ctext request.diffs with
    | Failure s -> Lwt.return (`Refused (cid, ctext))
    | Success ntext -> if rid = cid then
        begin
          let ncopy = { id = cid + 1;
                        text = ntext; } in
          ignore(append_copy ncopy);
          ignore(Eliom_bus.write bus (Patch (ruid, (Array.of_list rdiffs), (cid + 1))));
          Lwt.return (`Applied (cid + 1, ntext))
        end
      else begin Lwt.return (`Refused (cid, ctext)) end
  in
  get_copy ()
  >>= fun scopy ->
  verify_patch scopy

type patch_result = Failure of string
                  | Success of string

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
  Printf.printf "i = %d\ns = '%s'\ntext = '%s'\n" i s text;
  Printf.printf "Length(s) = %d\nLength(text) = %d\nI = %d\n" (String.length s) (String.length text) i;
  try
    let start = Str.string_before text (i + (String.length s)) in
    let chunk = Str.string_after start i in
    Printf.printf "c'%s' != s'%s'\n" chunk s;
    chunk = s
  with
  | Invalid_argument _ -> print_endline "false";false

let apply_diffs text diffs =
  let rtext = ref text in
  let rec inner diffs i =
    match diffs with
    | [] -> Success !rtext
    | (-1, str)::xs ->
      if apply_deletion rtext str i then
        inner xs i
      else
        begin
          Failure "Impossible to delete chunk"
        end
    | (0, str)::xs -> if check_coherence i str !rtext then inner xs (i + (String.length str))
      else Failure "Retain don't match :'"
    | (1, str)::xs ->
      if apply_addition rtext str i then
        inner xs (i + (String.length str))
      else
        Failure "Impossible to patch chunk"
    | _ -> Failure "Unknown patch operation"
  in inner diffs 0

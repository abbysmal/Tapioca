{shared{

open Eliom_content
open Html5.D
open Eliom_lib.Lwt_ops



type t =
  (Html5_types.div Eliom_content.Html5.elt * (Patches.bus_message, Patches.bus_message) Eliom_bus.t)

}}

{server{

let content =
  Html5.F.(
    div ~a:[a_contenteditable true; a_id "editor"]
      [span []])

let send_patch =
  Eliom_service.Ocaml.post_coservice'
    ~rt:(Eliom_service.rt :
           [`Applied of int * string | `Refused of int * string]
             Eliom_service.rt)
    ~post_params: (Eliom_parameter.ocaml "lol" Json.t<Patches.request>)
    ()

let patches_bus = Eliom_bus.create
    ~scope:Eliom_common.site_scope Json.t<Patches.bus_message>

}}

{client{

module Html = Dom_html
let (>>=) = Lwt.bind
open Dom


type phase =
  | Init of (int * diff * int) list
  | Ok
  | Disconnected


let load_document editor old rev =
  Eliom_client.call_ocaml_service ~service:%Services.get_document () ()
  >>= fun response ->
  begin
    match response with
    | `Result (document, id) ->
      editor##innerHTML <- (Js.string document);
      old := (Js.string document);
      rev := id; Lwt.return Ok
    | `NotConnected -> Lwt.return Disconnected
  end


let make_diff text old_text rev client_id =
  let dmp = DiffMatchPatch.make () in
  let diff = DiffMatchPatch.diff_main dmp old_text text in
  {from_revision = rev; diffs = (Array.to_list diff); client = client_id;}


(* Caret handling functions *)
let save_selection elt =
  let sel = Dom_html.window##getSelection () in
  let range = sel##getRangeAt(0) in
  let pre_range = range##cloneRange () in
  pre_range##selectNodeContents(elt);
  pre_range##setEnd(range##startContainer, range##startOffset);
  let start = (pre_range##toString())##length in
  (start, start + (range##toString())##length)

let get_length node =
  match Js.Opt.to_option (Dom.CoerceTo.text node)with
  | Some i -> i##length
  | None -> 0

(* Function used to get the whole text without markup *)
let get_text elt =
  let text = ref "" in
  let queue = Queue.create () in
  let rec inner queue node =
    if node##nodeType = Dom.TEXT
    then
      text := !text ^ (Js.to_string node##nodeValue);
    let len = node##childNodes##length in
    for i = 0 to (len - 1) do
      Queue.push (Js.Opt.get (node##childNodes##item (i)) (fun () -> assert false)) queue
    done;
    try inner queue (Queue.pop queue) with Queue.Empty -> () in
  inner queue elt;
  !text


let restore_selection elt (startpoint, endpoint) =
  let char_index = ref 0 in
  let range = Dom_html.document##createRange() in
  range##setStart(elt, 0);
  range##collapse(Js._true);
  let queue = Queue.create () in
  let foundstart = ref false in
  let stop = ref false in
  let rec inner queue node =
    if not !stop then
      if node##nodeType = Dom.TEXT
      then
        begin
          let next_index = !char_index + (get_length node) in
          if not !foundstart && (startpoint >= !char_index) && (startpoint <= next_index) then
            begin
              range##setStart(node, startpoint - !char_index);
              foundstart := true
            end;
          if !foundstart && (endpoint >= !char_index) && (endpoint <= next_index) then
            begin
              range##setEnd(node, endpoint - !char_index);
              stop := true
            end;
          char_index := next_index
        end
      else
        begin
          let max = node##childNodes##length in
          for i = 0 to (max - 1) do
            Queue.push (Js.Opt.get (node##childNodes##item (i)) (fun () -> assert false)) queue
          done;
        end;
      try inner queue (Queue.pop queue) with Queue.Empty -> ()
  in
  inner queue elt;
  let sel = Dom_html.window##getSelection() in
  sel##removeAllRanges();
  sel##addRange(range)

let apply_patches rev editor shadow_copy patches =
  List.iter (fun (id, diff, prev) ->
      if prev = !rev then
        let dmp = DiffMatchPatch.make () in
        let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
        let patch_editor = DiffMatchPatch.patch_make dmp (Js.to_string editor##innerHTML) diff in
        editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_editor (Js.to_string editor##innerHTML);
        shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
            dmp patch_scopy (Js.to_string !shadow_copy);
       rev := prev) (List.rev patches)


let apply_update rev editor shadow_copy diff prev =
  let current_text = Js.to_string editor##innerHTML in
  let dmp = DiffMatchPatch.make () in
  let patch_scopy = DiffMatchPatch.patch_make dmp (Js.to_string !shadow_copy) diff in
  let patch_editor = DiffMatchPatch.patch_make dmp (current_text) diff in
  (* let saved = save_selection (editor :> Dom.node Js.t) in *)
  editor##innerHTML <- Js.string @@ DiffMatchPatch.patch_apply dmp patch_editor current_text;
  shadow_copy := Js.string @@ DiffMatchPatch.patch_apply
      dmp patch_scopy (Js.to_string !shadow_copy);
  rev := prev(* ; *)
  (* restore_selection (editor :> Dom.node Js.t) saved *)


let onload patches_bus editor_elt () =
  Random.self_init ();

  (* Is the current revision server-side *)
  let editor = Eliom_content.Html5.To_dom.of_div editor_elt in
  let shadow_copy = ref (Js.string "") in
  (* Is the revision number of this client *)
  let rev = ref 0 in
  (* this client id *)
  let client_id = Random.int 4096 in
  let phase = ref (Init []) in

  let is_ok _ = match !phase with
    | Ok -> true
    | _ -> false in

  Lwt.async (fun _ -> Lwt_stream.iter
  (function
    | Hello id -> (* First, check if the bus is running
                     by checking our own Hello message *)
      if id = client_id then
        begin
          match !phase with
          | Init msg_buffer -> ignore begin
              load_document editor shadow_copy rev
              >>= function
              | Ok -> Lwt.return (phase := Ok)
              | _ -> Lwt.return (phase := Disconnected)
            end
          | _ -> ()
        end
    | Patch (id, diff, prev) when prev = (!rev + 1) ->
      begin
        try
          begin
            if id != client_id && is_ok () then
              begin
                apply_update rev editor shadow_copy diff prev
              end
            else if id != client_id then
              begin
                match !phase with
                | Init l -> phase := Init ((id, diff, prev)::l)
                | _ -> ()
              end
          end
        with
        | _ -> ()
      end
    | _ -> ()
  )
  (Eliom_bus.stream patches_bus));
  ignore(Eliom_bus.write patches_bus (Hello (client_id)));

  (* changes handler *)
  Lwt_js_events.(
    async
    (fun () ->
        inputs Dom_html.document
          (fun ev _ ->
             Lwt_js.sleep 0.3
             >>= fun () ->
             let diff = make_diff (Js.to_string editor##innerHTML)
                 (Js.to_string !shadow_copy) !rev client_id in
             Eliom_client.call_ocaml_service ~service:%send_patch () diff
             >>= fun response ->
             begin
               match response with
               | `Applied (srev, scopy) -> rev := srev;
                 shadow_copy := (Js.string scopy); Lwt.return_unit
               | `Refused (srev, scopy) -> Lwt.return ()
             end
          )))
}}
{server{

let create _ =
  let patches_bus = Eliom_bus.create
      ~scope:Eliom_common.site_scope Json.t<Patches.bus_message>
  in
  let elt = Eliom_content.Html5.D.div ~a:
      [a_contenteditable true] [] in
  (elt, patches_bus)

let init_and_register ((elt, bus): t) eref =
  let append_shadowcopy, get_shadowcopy =
    ((fun elm -> Eliom_reference.set eref elm),
     (fun () -> Eliom_reference.get eref)) in

  let handler = Patches.handle_patch_request get_shadowcopy append_shadowcopy bus in
  Eliom_registration.Ocaml.register
    ~service:send_patch
    (fun () patch ->
       handler patch);

  let get_document name = get_shadowcopy ()
    >>= fun {id = id; text = scopy} ->
    Lwt.return (`Result (scopy, id)) in

  Eliom_registration.Ocaml.register
    ~service:Services.get_document
    (fun () () ->
  get_document ());
  ignore {unit Lwt.t{
      Lwt_js.sleep 0.3 >>= (fun () -> Lwt.return (onload %bus %elt ()))
  }};
  Lwt.return_unit

let get_elt (elt, _) = elt

}}

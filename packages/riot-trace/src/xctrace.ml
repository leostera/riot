open Std
open Std.Result.Syntax

type cost_acc = {
  mutable self_samples: int;
  mutable total_samples: int;
  mutable self_weight_ns: int;
  mutable total_weight_ns: int;
}

type tree_acc = {
  name: string;
  mutable self_samples: int;
  mutable total_samples: int;
  mutable self_weight_ns: int;
  mutable total_weight_ns: int;
  children: (string, tree_acc) Collections.HashMap.t;
}

type sample = {
  weight_ns: int;
  stack: string list;
}

type trace_xml = {
  ids: (string, Data.Xml.t) Collections.HashMap.t;
}

let rec list_find_map = fun values ~fn ->
  match values with
  | [] -> None
  | value :: rest -> (
      match fn value with
      | Some _ as result -> result
      | None -> list_find_map rest ~fn
    )

let rec collect_elements = fun name node ->
  match node with
  | Data.Xml.Element { name = current_name; children; _ } ->
      let descendants =
        children
        |> List.flat_map ~fn:(collect_elements name)
      in
      if String.equal current_name name then
        node :: descendants
      else
        descendants
  | Data.Xml.Text _
  | Data.Xml.CData _ -> []

let element_name = fun __tmp1 ->
  match __tmp1 with
  | Data.Xml.Element { name; _ } -> Some name
  | Data.Xml.Text _
  | Data.Xml.CData _ -> None

let is_element = fun name node ->
  match element_name node with
  | Some actual -> String.equal actual name
  | None -> false

let index_ids = fun document ->
  let ids = Collections.HashMap.create () in
  let rec loop node =
    match node with
    | Data.Xml.Element { children; _ } ->
        Option.for_each
          (Data.Xml.attr "id" node)
          ~fn:(fun id ->
            let _ = Collections.HashMap.insert ids ~key:id ~value:node in
            ());
        List.for_each children ~fn:loop
    | Data.Xml.Text _
    | Data.Xml.CData _ -> ()
  in
  loop document;
  { ids }

let resolve = fun trace node ->
  match Data.Xml.attr "ref" node with
  | Some ref_ -> Option.unwrap_or ~default:node (Collections.HashMap.get trace.ids ~key:ref_)
  | None -> node

let child = fun trace name node ->
  resolve trace node
  |> Data.Xml.children
  |> list_find_map
    ~fn:(fun child ->
      let child = resolve trace child in
      if is_element name child then
        Some child
      else
        None)

let frame_has_binary = fun trace frame ->
  Option.is_some (child trace "binary" frame)

let is_hex_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let is_raw_address = fun value ->
  let len = String.length value in
  len > 2
  && String.starts_with ~prefix:"0x" value
  && String.sub value ~offset:2 ~len:(len - 2)
     |> String.for_all ~fn:is_hex_digit

let frame_name = fun trace frame ->
  let frame = resolve trace frame in
  match Data.Xml.attr "name" frame with
  | Some name -> name
  | None -> (
      match Data.Xml.attr "addr" frame with
      | Some addr -> addr
      | None -> "<unknown>"
    )

let frame_name_if_useful = fun trace frame ->
  let frame = resolve trace frame in
  let name = frame_name trace frame in
  if is_raw_address name && not (frame_has_binary trace frame) then
    None
  else
    Some name

let row_weight_ns = fun trace row ->
  match child trace "weight" row with
  | None -> 0
  | Some weight -> (
      match Int.parse (String.trim (Data.Xml.text_content (resolve trace weight))) with
      | None -> 0
      | Some weight_ns -> weight_ns
    )

let row_stack = fun trace row ->
  match child trace "tagged-backtrace" row with
  | None -> []
  | Some tagged_backtrace -> (
      match child trace "backtrace" tagged_backtrace with
      | None -> []
      | Some backtrace ->
          resolve trace backtrace
          |> Data.Xml.children
          |> List.filter_map
            ~fn:(fun child ->
              let child = resolve trace child in
              if is_element "frame" child then
                frame_name_if_useful trace child
              else
                None)
    )

let collect_samples = fun trace document ->
  collect_elements "row" document
  |> List.filter_map
    ~fn:(fun row ->
      let weight_ns = row_weight_ns trace row in
      let stack = row_stack trace row in
      if weight_ns > 0 && not (List.is_empty stack) then
        Some { weight_ns; stack }
      else
        None)

let add_total_cost = fun (table: (string, cost_acc) Collections.HashMap.t) ~name ~weight_ns ->
  match Collections.HashMap.get table ~key:name with
  | Some cost ->
      cost.total_samples <- cost.total_samples + 1;
      cost.total_weight_ns <- cost.total_weight_ns + weight_ns
  | None ->
      let cost: cost_acc = {
        self_samples = 0;
        total_samples = 1;
        self_weight_ns = 0;
        total_weight_ns = weight_ns;
      } in
      let _ =
        Collections.HashMap.insert
          table
          ~key:name
          ~value:cost
      in
      ()

let add_self_cost = fun (table: (string, cost_acc) Collections.HashMap.t) ~name ~weight_ns ->
  match Collections.HashMap.get table ~key:name with
  | Some cost ->
      cost.self_samples <- cost.self_samples + 1;
      cost.self_weight_ns <- cost.self_weight_ns + weight_ns
  | None ->
      let cost: cost_acc = {
        self_samples = 1;
        total_samples = 0;
        self_weight_ns = weight_ns;
        total_weight_ns = 0;
      } in
      let _ =
        Collections.HashMap.insert
          table
          ~key:name
          ~value:cost
      in
      ()

let cost_list = fun (table: (string, cost_acc) Collections.HashMap.t) ~sort_by ->
  Collections.HashMap.to_list table
  |> List.map
    ~fn:(fun (name, (cost: cost_acc)) -> (Profile.{
      name;
      samples = cost.self_samples;
      total_samples = cost.total_samples;
      self_weight_ns = cost.self_weight_ns;
      total_weight_ns = cost.total_weight_ns;
    }: Profile.call_cost))
  |> List.filter
    ~fn:(fun (cost: Profile.call_cost) ->
      match sort_by with
      | `Self -> cost.self_weight_ns > 0
      | `Total -> cost.total_weight_ns > 0)
  |> List.sort
    ~compare:(fun (left: Profile.call_cost) (right: Profile.call_cost) ->
      let left_weight =
        match sort_by with
        | `Self -> left.self_weight_ns
        | `Total -> left.total_weight_ns
      in
      let right_weight =
        match sort_by with
        | `Self -> right.self_weight_ns
        | `Total -> right.total_weight_ns
      in
      match Int.compare right_weight left_weight with
      | Order.EQ -> String.compare left.name right.name
      | diff -> diff)

let tree_node = fun name ->
  ({
    name;
    self_samples = 0;
    total_samples = 0;
    self_weight_ns = 0;
    total_weight_ns = 0;
    children = Collections.HashMap.create ();
  }: tree_acc)

let tree_child = fun parent name ->
  match Collections.HashMap.get parent.children ~key:name with
  | Some child -> child
  | None ->
      let child = tree_node name in
      let _ = Collections.HashMap.insert parent.children ~key:name ~value:child in
      child

let sorted_tree_children = fun node ->
  Collections.HashMap.to_list node.children
  |> List.map ~fn:(fun (_name, child) -> child)
  |> List.sort
    ~compare:(fun left right ->
      match Int.compare right.total_weight_ns left.total_weight_ns with
      | Order.EQ -> String.compare left.name right.name
      | diff -> diff)

let rec profile_tree_node = fun ~max_depth ~max_children ~depth node ->
  let children = sorted_tree_children node in
  let shown =
    if depth >= max_depth then
      []
    else
      List.take children ~len:max_children
  in
  let hidden_children = List.length children - List.length shown in
  Profile.{
    name = node.name;
    self_samples = node.self_samples;
    total_samples = node.total_samples;
    self_weight_ns = node.self_weight_ns;
    total_weight_ns = node.total_weight_ns;
    children =
      List.map shown ~fn:(profile_tree_node ~max_depth ~max_children ~depth:(depth + 1));
    hidden_children;
  }

let build_call_tree = fun samples ~max_depth ~max_children ->
  let root = tree_node "<root>" in
  List.for_each
    samples
    ~fn:(fun sample ->
      root.total_samples <- root.total_samples + 1;
      root.total_weight_ns <- root.total_weight_ns + sample.weight_ns;
      let node = ref root in
      List.for_each
        (List.reverse sample.stack)
        ~fn:(fun frame ->
          let child = tree_child !node frame in
          child.total_samples <- child.total_samples + 1;
          child.total_weight_ns <- child.total_weight_ns + sample.weight_ns;
          node := child);
      !node.self_samples <- !node.self_samples + 1;
      !node.self_weight_ns <- !node.self_weight_ns + sample.weight_ns);
  let children = sorted_tree_children root in
  let shown = List.take children ~len:max_children in
  (
    List.map shown ~fn:(profile_tree_node ~max_depth ~max_children ~depth:1),
    List.length children - List.length shown
  )

let summarize_time_profile_document = fun document ->
  let trace = index_ids document in
  let samples = collect_samples trace document in
  let costs = Collections.HashMap.create () in
  let sample_count = List.length samples in
  let total_weight_ns =
    List.fold_left samples ~init:0 ~fn:(fun total sample -> total + sample.weight_ns)
  in
  List.for_each
    samples
    ~fn:(fun sample ->
      match sample.stack with
      | [] -> ()
      | leaf :: _ ->
          add_self_cost costs ~name:leaf ~weight_ns:sample.weight_ns;
          let seen = Collections.HashSet.create () in
          List.for_each
            sample.stack
            ~fn:(fun name ->
              if Collections.HashSet.insert seen ~value:name then
                add_total_cost costs ~name ~weight_ns:sample.weight_ns));
  let (call_tree, hidden_call_tree_roots) =
    build_call_tree samples ~max_depth:10 ~max_children:12
  in
  Profile.{
    sample_count;
    total_weight_ns;
    top_self = cost_list costs ~sort_by:`Self;
    top_total = cost_list costs ~sort_by:`Total;
    call_tree;
    hidden_call_tree_roots;
  }

let summarize_time_profile_xml = fun xml ->
  match Data.Xml.from_string xml with
  | Ok document -> summarize_time_profile_document document
  | Error _ ->
      Profile.{
        sample_count = 0;
        total_weight_ns = 0;
        top_self = [];
        top_total = [];
        call_tree = [];
        hidden_call_tree_roots = 0;
      }

let export_time_profile = fun path ->
  let cmd =
    Command.make
      "xcrun"
      ~args:[
        "xctrace";
        "export";
        "--input";
        Path.to_string path;
        "--xpath";
        "/trace-toc/run/data/table[@schema=\"time-profile\"]";
      ]
  in
  match Command.output cmd with
  | Ok output when output.Command.status = 0 -> Ok output.Command.stdout
  | Ok output ->
      Error
        ("xctrace export failed with status "
        ^ Int.to_string output.Command.status
        ^ ": "
        ^ String.trim output.Command.stderr)
  | Error (Command.SystemError reason) -> Error ("failed to run xctrace export: " ^ reason)

let summarize_file = fun path ->
  let* xml = export_time_profile path in
  match Data.Xml.from_string xml with
  | Error err -> Error ("failed to parse xctrace XML: " ^ Data.Xml.error_message err)
  | Ok document -> Ok (summarize_time_profile_document document)

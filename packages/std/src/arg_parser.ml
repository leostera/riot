open Global
open IO
open Collections

type action =
  | Set
  | SetTrue
  | SetFalse
  | Append
  | Count

type 'a arg = {
  name: string;
  short: char option;
  long: string option;
  help: string option;
  value_name: string option;
  default: string option;
  required: bool;
  action: action;
  multiple: bool;
  env: string option;
  possible_values: string list option;
  conflicts_with: string list;
  requires: string list;
}

type command = {
  name: string;
  version: string option;
  about: string option;
  author: string option;
  args: unit arg list;
  subcommands: command list;
  subcommand_required: bool;
  allow_trailing: bool;
}

type matches = {
  command_name: string;
  values: (string, string list) HashMap.t;
  flags: (string, int) HashMap.t;
  mutable subcommand: (string * matches) option;
  mutable trailing_args: string list;
}

type error =
  | UnknownArgument of string
  | MissingRequired of string
  | InvalidValue of string * string
  | UnknownSubcommand of string
  | MissingSubcommand
  | ConflictingArguments of string * string
  | TooManyValues of string
  | TooFewValues of string

module Arg = struct
  type 'a t = 'a arg

  let make = fun name ->
    {
      name;
      short = None;
      long = None;
      help = None;
      value_name = None;
      default = None;
      required = false;
      action = Set;
      multiple = false;
      env = None;
      possible_values = None;
      conflicts_with = [];
      requires = [];
    }

  let flag = fun name -> { (make name) with action = SetTrue }

  let option = fun name -> make name

  let positional = fun name -> { (make name) with required = true }

  let trailing = fun name -> { (make name) with multiple = true }

  let short = fun c arg -> { arg with short = Some c }

  let long = fun s arg -> { arg with long = Some s }

  let help = fun s arg -> { arg with help = Some s }

  let value_name = fun s arg -> { arg with value_name = Some s }

  let default = fun v arg -> { arg with default = Some v }

  let required = fun b arg -> { arg with required = b }

  let env = fun s arg -> { arg with env = Some s }

  let action = fun a arg -> { arg with action = a }

  let multiple = fun arg -> { arg with multiple = true }

  let count = fun arg -> { arg with action = Count }

  let possible_values = fun vals arg -> { arg with possible_values = Some vals }

  let conflicts_with = fun name arg -> { arg with conflicts_with = name :: arg.conflicts_with }

  let requires = fun name arg -> { arg with requires = name :: arg.requires }
end

let command = fun name ->
  {
    name;
    version = None;
    about = None;
    author = None;
    args = [];
    subcommands = [];
    subcommand_required = true;
    allow_trailing = false;
  }

let version = fun v cmd -> { cmd with version = Some v }

let about = fun a cmd -> { cmd with about = Some a }

let author = fun a cmd -> { cmd with author = Some a }

let arg = fun a cmd -> { cmd with args = cmd.args @ [ a ] }

let args = fun a_list cmd -> { cmd with args = cmd.args @ a_list }

let subcommand = fun sub cmd -> { cmd with subcommands = cmd.subcommands @ [ sub ] }

let subcommands = fun sub_list cmd -> { cmd with subcommands = cmd.subcommands @ sub_list }

let allow_no_subcommand = fun cmd -> { cmd with subcommand_required = false }

let allow_trailing_args = fun cmd -> { cmd with allow_trailing = true }

let create_matches = fun name ->
  {
    command_name = name;
    values = HashMap.create ();
    flags = HashMap.create ();
    subcommand = None;
    trailing_args = [];
  }

let rec get_matches_internal = fun cmd args ->
  let matches = create_matches cmd.name in
  let validate_required () =
    let missing =
      List.find
        cmd.args
        ~fn:(fun arg ->
          arg.required && match HashMap.get matches.values ~key:arg.name with
          | None -> true
          | Some [] -> true
          | Some _ -> false)
    in
    match missing with
    | Some arg ->
        println ("Missing required argument: " ^ arg.name);
        println "";
        print_help cmd;
        System.exit 1
    | None -> Ok matches
  in
  let rec parse_args args_list =
    match args_list with
    | [] ->
        (* If command has subcommands but none provided, show help *)
        if List.length cmd.subcommands > 0 && cmd.subcommand_required then (
          print_help cmd;
          System.exit 0
        ) else
          validate_required ()
    | "--help" :: _
    | "-h" :: _ ->
        print_help cmd;
        System.exit 0
    | "--version" :: _ when Option.is_some cmd.version ->
        println (Option.unwrap cmd.version);
        System.exit 0
    | arg_str :: rest when String.starts_with ~prefix:"--" arg_str -> (
        let name = String.sub arg_str ~offset:2 ~len:(String.length arg_str - 2) in
        match find_arg_by_long cmd name with
        | Some arg -> parse_long_arg arg name rest
        | None ->
            if cmd.allow_trailing then (
              matches.trailing_args <- args_list;
              Ok matches
            ) else
              Error (UnknownArgument arg_str)
      )
    | arg_str :: rest when String.starts_with ~prefix:"-" arg_str && String.length arg_str > 1 -> (
        let c = String.get_unchecked arg_str ~at:1 in
        match find_arg_by_short cmd c with
        | Some arg -> parse_short_arg arg c rest
        | None ->
            if cmd.allow_trailing then (
              matches.trailing_args <- args_list;
              Ok matches
            ) else
              Error (UnknownArgument arg_str)
      )
    | subcmd :: rest -> (
        match List.find cmd.subcommands ~fn:(fun sub -> sub.name = subcmd) with
        | Some sub -> (
            match get_matches_internal sub rest with
            | Error err -> Error err
            | Ok sub_matches ->
                matches.subcommand <- Some (subcmd, sub_matches);
                Ok matches
          )
        | None ->
            (* Prefer consuming as positional when not a subcommand *)
            parse_positional args_list
      )
  and parse_long_arg arg name rest =
    match arg.action with
    | SetTrue ->
        let _ = HashMap.insert matches.flags ~key:name ~value:1 in
        parse_args rest
    | Count ->
        let count =
          HashMap.get matches.flags ~key:name
          |> Option.unwrap_or ~default:0
        in
        let _ = HashMap.insert matches.flags ~key:name ~value:(count + 1) in
        parse_args rest
    | Set
    | Append -> (
        match rest with
        | [] -> Error (InvalidValue (name, "missing value"))
        | value :: rest' ->
            let current =
              HashMap.get matches.values ~key:name
              |> Option.unwrap_or ~default:[]
            in
            let _ = HashMap.insert matches.values ~key:name ~value:(current @ [ value ]) in
            parse_args rest'
      )
    | SetFalse ->
        let _ = HashMap.insert matches.flags ~key:name ~value:0 in
        parse_args rest
  and parse_short_arg arg c rest = parse_long_arg arg arg.name rest
  and parse_positional pos_args =
    let positional_args: unit arg list =
      List.filter
        cmd.args
        ~fn:(fun positional_arg -> positional_arg.short = None && positional_arg.long = None)
    in
    let unfilled_positionals: unit arg list =
      List.filter
        positional_args
        ~fn:(fun (positional_arg: unit arg) ->
          let current = HashMap.get matches.values ~key:positional_arg.name in
          match current with
          | None -> true
          | Some [] -> true
          | Some _ -> positional_arg.multiple)
    in
    match (unfilled_positionals, pos_args) with
    | ([], []) -> validate_required ()
    | ([], value :: _) -> Error (UnknownArgument value)
    | (arg :: _, value :: rest) ->
        let current =
          HashMap.get matches.values ~key:arg.name
          |> Option.unwrap_or ~default:[]
        in
        let _ = HashMap.insert matches.values ~key:arg.name ~value:(current @ [ value ]) in
        parse_args rest
    | (arg :: _, []) when arg.required -> Error (MissingRequired arg.name)
    | (_, []) -> validate_required ()
  in
  parse_args args

and find_arg_by_long = fun cmd long_name ->
  List.find
    cmd.args
    ~fn:(fun arg -> arg.long = Some long_name)

and find_arg_by_short = fun cmd short_char ->
  List.find
    cmd.args
    ~fn:(fun arg -> arg.short = Some short_char)

and print_help = fun cmd ->
  (* Title/about on first line *)
  (
    match cmd.about with
    | Some a -> println (a ^ "\n")
    | None -> println (cmd.name ^ "\n")
  );
  (* Separate positional args from options *)
  let positionals = List.filter cmd.args ~fn:(fun arg -> arg.short = None && arg.long = None) in
  let options = List.filter cmd.args ~fn:(fun arg -> arg.short != None || arg.long != None) in
  (* Usage section *)
  let usage_buf = Buffer.create ~size:128 in
  Buffer.add_string usage_buf ("Usage: " ^ cmd.name);
  if List.length options > 0 then
    Buffer.add_string usage_buf " [OPTIONS]";
  List.for_each
    positionals
    ~fn:(fun arg ->
      let name =
        if arg.multiple then
          arg.name ^ "..."
        else
          arg.name
      in
      if arg.required then
        Buffer.add_string usage_buf (" <" ^ name ^ ">")
      else
        Buffer.add_string usage_buf (" [" ^ name ^ "]"));
  if cmd.allow_trailing then
    Buffer.add_string usage_buf " [-- ARGS...]";
  if List.length cmd.subcommands > 0 then
    Buffer.add_string usage_buf " [COMMAND]";
  println (Buffer.contents usage_buf);
  if List.length positionals > 0 then (
    println "\nArguments:";
    let max_arg_width: int =
      List.fold_left
        positionals
        ~init:0
        ~fn:(fun (acc: int) (arg: unit arg) ->
          let name =
            if arg.multiple then
              arg.name ^ "..."
            else
              arg.name
          in
          max acc (String.length name))
    in
    List.for_each
      positionals
      ~fn:(fun (arg: unit arg) ->
        let name =
          if arg.multiple then
            arg.name ^ "..."
          else
            arg.name
        in
        let arg_str =
          if arg.required then
            "<" ^ name ^ ">"
          else
            "[" ^ name ^ "]"
        in
        let padding_len = max 2 (max_arg_width - String.length name + 4) in
        let padding = String.make ~len:padding_len ~char:' ' in
        let help_str =
          match arg.help with
          | Some h -> h
          | None -> ""
        in
        println ("  " ^ arg_str ^ padding ^ help_str))
  );
  if List.length options > 0 then (
    println "\nOptions:";
    (* Calculate max width for alignment *)
    let max_opt_width =
      List.fold_left
        options
        ~init:0
        ~fn:(fun acc arg ->
          let short_len =
            match arg.short with
            | Some _ -> 4
            | None -> 0
          in
          let long_len =
            match arg.long with
            | Some l -> String.length l + 2
            | None -> 0
          in
          max acc (short_len + long_len))
    in
    List.for_each
      options
      ~fn:(fun arg ->
        let short_str =
          match arg.short with
          | Some c -> "-" ^ String.make ~len:1 ~char:c ^ ", "
          | None -> "    "
        in
        let long_str =
          match arg.long with
          | Some l -> "--" ^ l
          | None -> ""
        in
        let opt_str = short_str ^ long_str in
        let padding_len = max 2 (max_opt_width - String.length opt_str + 2) in
        let padding = String.make ~len:padding_len ~char:' ' in
        let help_str =
          match arg.help with
          | Some h -> h
          | None -> ""
        in
        println ("  " ^ opt_str ^ padding ^ help_str))
  );
  if List.length cmd.subcommands > 0 then (
    println "\nCommands:";
    (* Sort subcommands alphabetically *)
    let sorted_subs =
      List.sort cmd.subcommands ~compare:(fun a b -> String.compare a.name b.name)
    in
    let max_name_len =
      List.fold_left sorted_subs ~init:0 ~fn:(fun acc sub -> max acc (String.length sub.name))
    in
    List.for_each
      sorted_subs
      ~fn:(fun sub ->
        let padding = String.make ~len:(max_name_len - String.length sub.name + 4) ~char:' ' in
        let about_str =
          match sub.about with
          | Some a -> a
          | None -> ""
        in
        println ("    " ^ sub.name ^ padding ^ about_str));
    println
      ("\nSee '" ^ cmd.name ^ " <command> --help' for more information on a specific command.")
  )

let get_matches = fun cmd args ->
  match args with
  | [] -> get_matches_internal cmd []
  | _ :: rest -> get_matches_internal cmd rest

let get_one = fun matches name ->
  match HashMap.get matches.values ~key:name with
  | Some (v :: _) -> Some v
  | _ -> None

let get_flag = fun matches name ->
  (
    HashMap.get matches.flags ~key:name
    |> Option.unwrap_or ~default:0
  ) > 0

let get_count = fun matches name ->
  HashMap.get matches.flags ~key:name
  |> Option.unwrap_or ~default:0

let get_many = fun matches name ->
  HashMap.get matches.values ~key:name
  |> Option.unwrap_or ~default:[]

let get_int = fun matches name ->
  match get_one matches name with
  | Some s -> Int.parse s
  | None -> None

let get_float = fun matches name ->
  match get_one matches name with
  | Some s -> Float.parse s
  | None -> None

let get_path = fun matches name ->
  match get_one matches name with
  | Some s -> (
      match Path.from_string s with
      | Ok path -> Some path
      | Error _ -> None
    )
  | None -> None

let get_subcommand = fun matches -> matches.subcommand

let subcommand_name = fun matches ->
  match matches.subcommand with
  | Some (name, _) -> Some name
  | None -> None

let subcommand_matches = fun matches name ->
  match matches.subcommand with
  | Some (n, m) when n = name -> Some m
  | _ -> None

let trailing_args = fun matches -> matches.trailing_args

let error_message = fun __tmp1 ->
  match __tmp1 with
  | UnknownArgument arg -> "Unknown argument: " ^ arg
  | MissingRequired name -> "Missing required argument: " ^ name
  | InvalidValue (name, msg) -> "Invalid value for " ^ name ^ ": " ^ msg
  | UnknownSubcommand name -> "Unknown subcommand: " ^ name
  | MissingSubcommand -> "Missing subcommand"
  | ConflictingArguments (a, b) -> "Conflicting arguments: " ^ a ^ " and " ^ b
  | TooManyValues name -> "Too many values for: " ^ name
  | TooFewValues name -> "Too few values for: " ^ name

let print_error = fun err -> println ("error: " ^ error_message err)

let usage_string = fun cmd ->
  let buf = Buffer.create ~size:256 in
  Buffer.add_string buf ("Usage: " ^ cmd.name);
  (* Add options if any *)
  let has_options = List.any cmd.args ~fn:(fun arg -> arg.short != None || arg.long != None) in
  if has_options then
    Buffer.add_string buf " [OPTIONS]";
  let positionals = List.filter cmd.args ~fn:(fun arg -> arg.short = None && arg.long = None) in
  List.for_each
    positionals
    ~fn:(fun arg ->
      let name =
        if arg.multiple then
          arg.name ^ "..."
        else
          arg.name
      in
      if arg.required then
        Buffer.add_string buf (" <" ^ name ^ ">")
      else
        Buffer.add_string buf (" [" ^ name ^ "]"));
  (* Add subcommands indicator *)
  if List.length cmd.subcommands > 0 then
    Buffer.add_string buf " [COMMAND]";
  Buffer.contents buf

let print_usage = fun cmd -> println (usage_string cmd)

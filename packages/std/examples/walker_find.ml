open Std
open Std.Iter

type config = {
  count_only: bool;
  sort: bool;
  roots: Path.t list;
}

type parse_result =
  | Help
  | Config of config

let default_config = { count_only = false; sort = false; roots = [ Path.v "." ] }

let ignored_paths = [
  ".git";
  ".git/**";
  "**/.git";
  "**/.git/**";
  ".worktrees";
  ".worktrees/**";
  "**/.worktrees";
  "**/.worktrees/**"
]

let ignored_globs = Glob.create ignored_paths |> Result.expect ~msg:"walker_find ignore globs should compile"

let should_ignore = fun entry ->
  Glob.matches ignored_globs ~str:(Fs.Walker.FileItem.path_string entry)
  |> Result.unwrap_or ~default:false

let print_usage = fun () ->
  eprintln "usage: walker_find [--count-only] [--sort] [root ...]";
  eprintln "ignores: .git and .worktrees"

let rec parse_args = fun config ->
  function
  | [] ->
      Config config
  | "--count-only" :: rest ->
      parse_args { config with count_only = true } rest
  | "--sort" :: rest ->
      parse_args { config with sort = true } rest
  | "--help" :: _
  | "-h" :: _ ->
      Help
  | root :: rest ->
      let roots =
        if config.roots = default_config.roots then
          [ Path.v root ]
        else
          config.roots @ [ Path.v root ]
      in
      parse_args { config with roots } rest

let main = fun ~args ->
  let args =
    match args with
    | [] -> []
    | _exe :: rest -> rest
  in
  match parse_args default_config args with
  | Help ->
      print_usage ();
      Ok ()
  | Config config ->
      let walker = Fs.Walker.create ~roots:config.roots ~sort:config.sort ()
      |> Result.expect ~msg:"walker_find configuration should be valid"
      |> Fs.Walker.filter_entry ~f:(fun entry -> not (should_ignore entry)) in
      let iter = Fs.Walker.into_iter walker in
      let count = ref 0 in
      let rec loop iter =
        match Iterator.next iter with
        | None, _ ->
            Ok ()
        | Some (Error (err: Fs.Walker.error)), iter' ->
            eprintln
              ("walker error at depth " ^ string_of_int err.depth ^ ": " ^ IO.error_message err.cause);
            loop iter'
        | Some (Ok (entry: Fs.Walker.FileItem.t)), iter' ->
            count := !count + 1;
            if not config.count_only then
              println (Fs.Walker.FileItem.path_string entry);
            loop iter'
      in
      loop iter |> Result.map
        (fun () ->
          if config.count_only then
            println (string_of_int !count))

let () = Runtime.run ~main ~args:Env.args ()

open Std

let compiler_output = fun result -> Riot_toolchain.Ocamlc.get_output result

let link_shared_library = fun ?cwd (session: Context.session) ~output ~libs objects ->
  let ocamlc = Riot_toolchain.ocamlc session.toolchain in
  let cwd =
    match cwd with
    | Some cwd -> cwd
    | None -> session.session_dir
  in
  Riot_toolchain.Ocamlc.create_shared_library
    ocamlc
    ~cwd
    ~includes:session.includes
    ~output
    ~libs
    objects
  |> Riot_toolchain.Ocamlc.run

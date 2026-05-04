open Global

type ctx = Test_context.t

type test_result =
  | Pass
  | Fail of string
  | Error of exn

type test_type =
  | UnitTest
  | Property of { examples: int }
  | Fuzz of { seeds: int }

type size =
  | Small
  | Large

type reliability =
  | Stable
  | Flaky of { retry_attempts: int }

type t = {
  name: string;
  test_type: test_type;
  size: size;
  reliability: reliability;
  fn: ctx -> (unit, string) result;
  fuzz_fn: (ctx -> string -> (unit, string) result) option;
  fuzz_corpus: Fuzz.Corpus.t option;
  fuzz_mutator: Fuzz.Mutator.t option;
  skip: bool;
}

let case = fun ?(size = Small) ?(reliability = Stable) name fn ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn;
    fuzz_fn = None;
    fuzz_corpus = None;
    fuzz_mutator = None;
    skip = false;
  }

let property = fun ?(size = Small) ?(reliability = Stable) name ~examples fn ->
  {
    name;
    test_type = Property { examples };
    size;
    reliability;
    fn;
    fuzz_fn = None;
    fuzz_corpus = None;
    fuzz_mutator = None;
    skip = false;
  }

let slugify = fun value ->
  let is_slug_char char =
    let code = Char.to_int char in
    (code >= Char.to_int 'a' && code <= Char.to_int 'z')
    || (code >= Char.to_int 'A' && code <= Char.to_int 'Z')
    || (code >= Char.to_int '0' && code <= Char.to_int '9')
    || Char.equal char '-'
    || Char.equal char '_'
  in
  let bytes = IO.Bytes.from_string value in
  for idx = 0 to IO.Bytes.length bytes - 1 do
    let char = IO.Bytes.get_unchecked bytes ~at:idx in
    let char =
      if is_slug_char char then
        Char.lowercase_ascii char
      else
        '_'
    in
    IO.Bytes.set_unchecked bytes ~at:idx ~char
  done;
  IO.Bytes.to_string bytes

let compare_path = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let path_in_dir = fun dir path ->
  if Path.is_absolute path then
    path
  else
    Path.(dir / path)

let fuzz_case_dir = fun (ctx: ctx) ->
  match (ctx.workspace_root, ctx.package_name) with
  | (Some workspace_root, Some package_name) ->
      Some Path.(workspace_root
      / Path.v ".riot"
      / Path.v "fuzzing"
      / Path.v package_name
      / Path.v (slugify ctx.suite_name)
      / Path.v (slugify ctx.test_name))
  | _ -> None

let read_fuzz_inputs_from_dir = fun label dir ->
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok reader ->
      Iter.MutIterator.to_list reader
      |> Collections.List.sort ~compare:compare_path
      |> Collections.List.filter_map
        ~fn:(fun path ->
          let path = path_in_dir dir path in
          match Fs.is_file path with
          | Ok true -> (
              match Fs.read path with
              | Ok input -> Some (label ^ "/" ^ Path.basename path, input)
              | Error _ -> None
            )
          | Ok false
          | Error _ -> None)

let fuzz_replay_inputs = fun ctx corpus ->
  let seed_inputs = Fuzz.Corpus.replay_inputs corpus in
  match fuzz_case_dir ctx with
  | None -> seed_inputs
  | Some dir ->
      let corpus_inputs = read_fuzz_inputs_from_dir "corpus" Path.(dir / Path.v "corpus") in
      let crash_inputs = read_fuzz_inputs_from_dir "crashes" Path.(dir / Path.v "crashes") in
      (seed_inputs @ corpus_inputs) @ crash_inputs

let fuzz = fun
  ?(size = Small)
  ?(reliability = Stable)
  ?(seeds = [""])
  ?(corpus = Fuzz.Corpus.empty)
  ?(mutator = Fuzz.Mutator.bytes)
  name
  fuzz_fn ->
  let seeds =
    match seeds with
    | [] -> [ "" ]
    | seeds -> seeds
  in
  let fuzz_corpus = Fuzz.Corpus.merge [ Fuzz.Corpus.bytes seeds; corpus ] in
  let fn ctx =
    let rec loop index = fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok ()
      | (origin, input) :: rest -> (
          let result =
            try fuzz_fn ctx input with
            | exn -> Result.Error (Exception.to_string exn)
          in
          match result with
          | Ok () -> loop (index + 1) rest
          | Error message -> Result.Error ("fuzz input " ^ origin ^ " failed: " ^ message)
        )
    in
    loop 1 (fuzz_replay_inputs ctx fuzz_corpus)
  in
  {
    name;
    test_type = Fuzz { seeds = Collections.List.length seeds };
    size;
    reliability;
    fn;
    fuzz_fn = Some fuzz_fn;
    fuzz_corpus = Some fuzz_corpus;
    fuzz_mutator = Some mutator;
    skip = false;
  }

let skip = fun ?(size = Small) ?(reliability = Stable) name fn ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn;
    fuzz_fn = None;
    fuzz_corpus = None;
    fuzz_mutator = None;
    skip = true;
  }

let todo = fun ?(size = Small) ?(reliability = Stable) name ->
  {
    name;
    test_type = UnitTest;
    size;
    reliability;
    fn = (fun _ctx -> Result.Error "todo");
    fuzz_fn = None;
    fuzz_corpus = None;
    fuzz_mutator = None;
    skip = false;
  }

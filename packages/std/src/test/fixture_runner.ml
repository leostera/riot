open Global
open Collections

type ctx = {
  test: Test_context.t;
  fixture_path: Path.t;
  fixture_relpath: string;
  fixture_name: string;
}

let is_snapshot_artifact = fun path ->
  let basename = Path.basename path in
  String.ends_with ~suffix:".expected" basename
  || String.ends_with ~suffix:".expected.new" basename

let discover_fixture_paths = fun root ->
  let rec visit = fun path ->
    match Fs.is_dir path with
    | Error err -> Error err
    | Ok false ->
        if is_snapshot_artifact path then
          Ok []
        else
          Ok [ path ]
    | Ok true -> (
        match Fs.read_dir path with
        | Error err -> Error err
        | Ok entries ->
            let children = Iter.MutIterator.to_list entries in
            let rec loop acc = function
              | [] -> Ok acc
              | child :: rest -> (
                  let child_path = Path.join path child in
                  match visit child_path with
                  | Error err -> Error err
                  | Ok discovered -> loop (List.rev_append discovered acc) rest
                )
            in
            loop [] children
      )
  in
  match visit root with
  | Error err -> Error err
  | Ok found ->
      Ok
        (found
         |> List.sort
           (fun left right ->
             String.compare (Path.to_string left) (Path.to_string right)))

let relpath_string = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok relpath -> Path.to_string relpath
  | Error _ -> Path.to_string path

let fixture_name = fun relpath ->
  relpath |> Path.v |> Path.remove_extension |> Path.to_string

let cases = fun ~dir ~run ->
  let root =
    let root = Path.v dir in
    if Path.is_relative root then
      match Env.current_dir () with
      | Ok cwd -> Path.join cwd root
      | Error _ -> root
    else
      root
  in
  let fixtures =
    match discover_fixture_paths root with
    | Ok fixtures -> fixtures
    | Error err ->
        panic
          ("failed to discover fixtures under "
          ^ Path.to_string root
          ^ ": "
          ^ IO.error_message err)
  in
  fixtures |> List.map
    (fun fixture_path ->
      let fixture_relpath = relpath_string ~root fixture_path in
      let fixture_name = fixture_name fixture_relpath in
      Test_case.case fixture_relpath
        (fun test_ctx ->
          let fixture = Test_context.{ path = fixture_path; relpath = fixture_relpath; name = fixture_name } in
          run
            {
              test = Test_context.with_fixture test_ctx fixture;
              fixture_path;
              fixture_relpath;
              fixture_name;
            }))

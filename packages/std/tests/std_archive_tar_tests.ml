open Std
open Std.Archive

let tar_block_size = 512

let bytes_set_string = fun dst ~offset ~width value ->
  let bytes = IO.Bytes.of_string value in
  let copy_len = min width (IO.Bytes.length bytes) in
  IO.Bytes.blit bytes 0 dst offset copy_len

let octal_string = fun value ->
  let rec loop acc remaining =
    if Int64.equal remaining 0L then
      acc
    else
      let digit = Int64.to_int (Int64.rem remaining 8L) in
      let ch = Char.chr (Char.code '0' + digit) in
      loop (String.make 1 ch ^ acc) (Int64.div remaining 8L)
  in
  if Int64.equal value 0L then
    "0"
  else
    loop "" value

let zero_pad_left = fun width value ->
  if String.length value >= width then
    String.sub value (String.length value - width) width
  else
    String.make (width - String.length value) '0' ^ value

let bytes_set_octal = fun dst ~offset ~width value ->
  let digits_width = max 1 (width - 1) in
  let trimmed = zero_pad_left digits_width (octal_string value) in
  bytes_set_string dst ~offset ~width:((width - 1)) trimmed;
  IO.Bytes.set dst (offset + width - 1) '\000'

let compute_checksum = fun header ->
  let sum = ref 0 in
  for index = 0 to tar_block_size - 1 do
    sum := !sum + Char.code (IO.Bytes.get header index)
  done;
  !sum

let make_header = fun ~name ~kind ~mode ~size ->
  let header = IO.Bytes.make tar_block_size '\000' in
  bytes_set_string header ~offset:0 ~width:100 name;
  bytes_set_octal header ~offset:100 ~width:8 mode;
  bytes_set_octal header ~offset:108 ~width:8 0L;
  bytes_set_octal header ~offset:116 ~width:8 0L;
  bytes_set_octal header ~offset:124 ~width:12 size;
  bytes_set_octal header ~offset:136 ~width:12 0L;
  bytes_set_string header ~offset:148 ~width:8 "        ";
  IO.Bytes.set header 156 kind;
  bytes_set_string header ~offset:257 ~width:6 "ustar";
  bytes_set_string header ~offset:263 ~width:2 "00";
  let checksum = compute_checksum header in
  let checksum_field = zero_pad_left 6 (octal_string (Int64.of_int checksum)) ^ "\000 " in
  bytes_set_string header ~offset:148 ~width:8 checksum_field;
  header

let pad_data = fun data ->
  let len = String.length data in
  let remainder = Int.rem len tar_block_size in
  if remainder = 0 then
    ""
  else
    String.make (tar_block_size - remainder) '\000'

let build_archive = fun entries ->
  let buffer = IO.Buffer.create 2_048 in
  List.iter
    (fun (name, kind, mode, data) ->
      let size = Int64.of_int (String.length data) in
      IO.Buffer.add_bytes buffer (make_header ~name ~kind ~mode ~size);
      IO.Buffer.add_string buffer data;
      IO.Buffer.add_string buffer (pad_data data))
    entries;
  IO.Buffer.add_string buffer (String.make (tar_block_size * 2) '\000');
  IO.Buffer.contents buffer

let test_entries_lists_archive_members = fun _ctx ->
  let archive = build_archive
    [ ("src/", '5', 0o755L, ""); ("src/hello.txt", '0', 0o644L, "hello tar\n"); ] in
  match Tar.entries (IO.Reader.from_string archive) with
  | Error _ -> Error "failed to list tar entries"
  | Ok entries ->
      let paths =
        List.map (fun (entry: Tar.entry) -> Path.to_string entry.path) entries
      in
      if paths = [ "src/"; "src/hello.txt" ] || paths = [ "src"; "src/hello.txt" ] then
        Ok ()
      else
        Error ("unexpected tar entries: " ^ String.concat ", " paths)

let with_temp_dir = fun label fn ->
  let temp_root = Path.join
    (Path.v "/tmp")
    (Path.v ("riot_" ^ label ^ "_" ^ UUID.to_string (UUID.v4 ()))) in
  match Fs.create_dir_all temp_root with
  | Error err -> Error ("failed to create temp dir: " ^ Kernel.IO.error_message err)
  | Ok () -> Kernel.Fun.protect
    ~finally:(fun () -> ignore (Fs.remove_dir_all temp_root))
    (fun () -> fn temp_root)

let test_extract_writes_regular_files = fun _ctx ->
  let archive = build_archive
    [ ("pkg/", '5', 0o755L, ""); ("pkg/README.md", '0', 0o644L, "Hello from tar\n"); ] in
  with_temp_dir "tar_extract"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive"
      | Ok () -> (
          let readme = Path.join (Path.join dir (Path.v "pkg")) (Path.v "README.md") in
          match Fs.read_to_string readme with
          | Ok content when content = "Hello from tar\n" -> Ok ()
          | Ok content -> Error ("unexpected extracted content: " ^ content)
          | Error err -> Error ("failed to read extracted file: " ^ Kernel.IO.error_message err)
        ))

let test_extract_allows_dot_root_directory_entry = fun _ctx ->
  let archive = build_archive
    [
      ("./", '5', 0o755L, "");
      ("./src/", '5', 0o755L, "");
      ("./src/std.ml", '0', 0o644L, "let answer = 42\n");
    ] in
  with_temp_dir "tar_dot_root"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive with dot root entry"
      | Ok () -> (
          let extracted = Path.join (Path.join dir (Path.v "src")) (Path.v "std.ml") in
          match Fs.read_to_string extracted with
          | Ok "let answer = 42\n" -> Ok ()
          | Ok text -> Error ("unexpected extracted dot-root content: " ^ text)
          | Error err -> Error ("failed to read dot-root extracted file: "
          ^ Kernel.IO.error_message err)
        ))

let test_extract_rejects_path_traversal = fun _ctx ->
  let archive = build_archive [ ("../escape.txt", '0', 0o644L, "bad"); ] in
  with_temp_dir "tar_traversal"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error (Tar.Extract_error (Tar.Unsafe_path _)) -> Ok ()
      | Error _ -> Error "expected unsafe-path rejection"
      | Ok () -> Error "tar extraction should reject path traversal")

let tests =
  Test.[
    case "tar entries lists archive members" test_entries_lists_archive_members;
    case "tar extract writes regular files" test_extract_writes_regular_files;
    case "tar extract allows dot root directory entry" test_extract_allows_dot_root_directory_entry;
    case "tar extract rejects path traversal" test_extract_rejects_path_traversal;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"std_archive_tar" ~tests ~args)
    ~args:Env.args
    ()

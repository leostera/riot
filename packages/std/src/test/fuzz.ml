open Global

module Corpus = struct
  type dir = {
    path: Path.t;
    extensions: string list;
  }

  type source =
    | Inline of string
    | File of Path.t
    | Dir of dir

  type t = source list

  let empty = []

  let bytes = fun inputs -> Collections.List.map inputs ~fn:(fun input -> Inline input)

  let strings = bytes

  let file = fun path -> [ File path ]

  let files = fun paths -> Collections.List.map paths ~fn:(fun path -> File path)

  let dir = fun ?(extensions = []) path -> [ Dir { path; extensions } ]

  let merge = fun corpuses -> Collections.List.concat corpuses

  let inline_inputs = fun corpus ->
    Collections.List.filter_map
      corpus
      ~fn:(fun source ->
        match source with
        | Inline input -> Some input
        | File _
        | Dir _ -> None)

  let extension_matches = fun extensions path ->
    Collections.List.is_empty extensions || match Path.extension path with
    | Some extension ->
        Collections.List.any extensions ~fn:(fun expected -> String.equal expected extension)
    | None -> false

  let files_in_dir = fun { path; extensions } ->
    match Fs.Walker.to_list ~roots:[ path ] ~include_directories:false () with
    | Error _ -> []
    | Ok items ->
        items
        |> Collections.List.filter_map
          ~fn:(fun item ->
            match Fs.Walker.FileItem.kind item with
            | Fs.Walker.File ->
                let path = Fs.Walker.FileItem.path item in
                if extension_matches extensions path then
                  Some path
                else
                  None
            | Fs.Walker.Directory
            | Fs.Walker.Symlink
            | Fs.Walker.Other -> None)

  let file_paths = fun corpus ->
    corpus
    |> Collections.List.flat_map
      ~fn:(fun source ->
        match source with
        | Inline _ -> []
        | File path -> [ path ]
        | Dir dir -> files_in_dir dir)

  let replay_inputs = fun corpus ->
    let inline =
      inline_inputs corpus
      |> Collections.List.enumerate
      |> Collections.List.map ~fn:(fun (idx, input) -> ("seed " ^ Int.to_string (idx + 1), input))
    in
    let files =
      file_paths corpus
      |> Collections.List.filter_map
        ~fn:(fun path ->
          match Fs.read path with
          | Ok input -> Some ("file " ^ Path.to_string path, input)
          | Error _ -> None)
    in
    inline @ files
end

module Mutator = struct
  type t = {
    dictionary: string list;
    max_len: int option;
    splicing: bool;
  }

  let bytes = { dictionary = []; max_len = None; splicing = true }

  let text = { bytes with dictionary = [ ""; " "; "\n"; "\t"; "\000"; "\""; "'"; "\\"; ] }

  let normalize_dictionary = fun dictionary ->
    dictionary
    |> Collections.List.filter ~fn:(fun token -> not (String.equal token ""))
    |> Collections.List.unique ~compare:String.compare

  let dictionary = fun dictionary -> { bytes with dictionary = normalize_dictionary dictionary }

  let with_dictionary = fun dictionary mutator -> {
    mutator with
    dictionary = normalize_dictionary (mutator.dictionary @ dictionary);
  }

  let with_max_len = fun max_len mutator -> { mutator with max_len = Some (Int.max 1 max_len) }

  let with_splicing = fun mutator -> { mutator with splicing = true }

  let without_splicing = fun mutator -> { mutator with splicing = false }
end

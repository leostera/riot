open Std

type analysis_slot = {
  source_id: SourceId.t;
  source: Source.t;
  config: TypConfig.t;
  mutable analysis: SourceAnalysis.t option;
}

type t = {
  revision: int;
  analyses: analysis_slot list;
}

let make = fun ~revision ~config ~sources ->
  let analyses =
    sources
    |> List.map (fun (source: Source.t) ->
      {
        source_id = source.source_id;
        source;
        config;
        analysis = None;
      })
  in
  { revision; analyses }

let force_analysis = fun (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
      let analysis = SourceAnalysis.analyze ~config:slot.config slot.source in
      let () = slot.analysis <- Some analysis in
      analysis

let revision = fun snapshot ->
  snapshot.revision

let analyses = fun snapshot ->
  snapshot.analyses
  |> List.map force_analysis

let find_analysis = fun snapshot source_id ->
  List.find_opt
    (fun (slot: analysis_slot) -> SourceId.equal slot.source_id source_id)
    snapshot.analyses
  |> function
  | Some slot -> Some (force_analysis slot)
  | None -> None

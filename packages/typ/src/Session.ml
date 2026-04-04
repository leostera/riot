open Std

type t = {
  config: TypConfig.t;
  next_source_id: int;
  next_revision: int;
  sources: Source.t list;
}

let empty = fun ~config ->
  {
    config;
    next_source_id = 0;
    next_revision = 0;
    sources = [];
  }

let config = fun session ->
  session.config

let create_source = fun session ~kind ~origin ~text ->
  let source_id = SourceId.of_int session.next_source_id in
  let source = Source.make ~source_id ~kind ~origin ~revision:session.next_revision ~text in
  ({
     session with
     next_source_id = session.next_source_id + 1;
     next_revision = session.next_revision + 1;
     sources = session.sources @ [ source ];
   },
   source_id)

let update_source_text = fun session source_id ~text ->
  let revision = session.next_revision in
  let sources =
    session.sources
    |> List.map (fun (source: Source.t) ->
      if SourceId.equal source.source_id source_id then
        Source.update_text source ~revision ~text
      else
        source)
  in
  { session with next_revision = revision + 1; sources }

let remove_source = fun session source_id ->
  {
    session with
    next_revision = session.next_revision + 1;
    sources = List.filter (fun (source: Source.t) -> not (SourceId.equal source.source_id source_id)) session.sources;
  }

let snapshot = fun session ->
  Snapshot.make ~revision:session.next_revision ~config:session.config ~sources:session.sources

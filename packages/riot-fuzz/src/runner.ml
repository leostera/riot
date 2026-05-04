open Std
open Std.Result.Syntax
open Types

module Queue = Collections.Queue

let status_failed = fun __tmp1 ->
  match __tmp1 with
  | Afl.Exited 0 -> false
  | Afl.Exited _
  | Afl.Signaled _
  | Afl.Stopped _
  | Afl.Timed_out _ -> true

let target_env = fun target ->
  if List.exists (fun (name, _value) -> String.equal name "RIOT_SCHEDULERS") target.env then
    target.env
  else
    ("RIOT_SCHEDULERS", "1") :: target.env

let stop_forkserver = fun forkserver ->
  match forkserver with
  | None -> Ok ()
  | Some forkserver -> Afl.stop_forkserver forkserver

let close_map = fun map ->
  match map with
  | None -> Ok ()
  | Some map -> Afl.close_map map

let load_declared_file_inputs = fun files ->
  files
  |> List.filter_map
    ~fn:(fun path ->
      match Fs.read path with
      | Ok input -> Some input
      | Error _ -> None)

let effective_max_len = fun (request: request) ->
  match request.mutator.max_len with
  | Some max_len -> Int.min request.max_len max_len
  | None -> request.max_len

let elapsed_millis = fun started_at ->
  Time.Instant.elapsed started_at
  |> Time.Duration.to_millis

let duration_ms = fun duration -> Option.map duration ~fn:Time.Duration.to_millis

let duration_elapsed = fun started_at duration ->
  match duration with
  | None -> false
  | Some duration -> Time.Duration.compare (Time.Instant.elapsed started_at) duration != Order.LT

let should_emit_progress = fun ~last_progress_ms ~run_index ~elapsed_ms ->
  run_index = 1 || run_index mod 1_000 = 0 || elapsed_ms - !last_progress_ms >= 2_000

let triage_crash = fun (request: request) ~run ~input_path ~status ->
  match Capture.run ~target:request.target ~input_path ~timeout_ms:request.timeout_ms with
  | Error _ -> Ok ()
  | Ok capture ->
      let* artifacts =
        Corpus.save_crash_artifacts
          ~case_dir:request.case_dir
          ~crash_path:input_path
          ~status:capture.status
          ~stdout:capture.stdout
          ~stderr:capture.stderr
      in
      request.on_event
        (
          Crash_triaged {
            run;
            input_path;
            stdout_path = artifacts.stdout_path;
            stderr_path = artifacts.stderr_path;
            status_path = artifacts.status_path;
            status = capture.status;
          }
        );
      Ok ()

let execute_with_forkserver = fun ~target ~input_path ~map ~forkserver_ref ~timeout_ms ->
  let* () = Afl.reset_map map in
  let* () =
    match !forkserver_ref with
    | None ->
        let* started =
          Afl.start_forkserver
            ~program:target.program
            ~args:(target.args ~input_path)
            ?cwd:target.cwd
            ~env:(target_env target)
            map
        in
        forkserver_ref := Some started;
        Ok ()
    | Some forkserver -> Afl.start_next_run forkserver
  in
  let* forkserver =
    match !forkserver_ref with
    | Some forkserver -> Ok forkserver
    | None -> Error (Error.Native_error 0)
  in
  let* status = Afl.finish_run ~timeout_ms forkserver in
  let snapshot = Afl.snapshot_map map in
  Ok (status, snapshot)

let run = fun (request: request) ->
  let* (corpus_dir, crashes_dir) = Corpus.ensure_case_dirs request.case_dir in
  let* () = Corpus.seed_empty corpus_dir in
  let declared_corpus = request.corpus.inputs @ load_declared_file_inputs request.corpus.files in
  let corpus =
    match declared_corpus @ Corpus.load corpus_dir with
    | [] -> [ "" ]
    | corpus -> corpus
  in
  let* rng =
    Random.Rng.standard ?seed:request.seed ()
    |> Result.map_err ~fn:(fun err -> Error.Random_error (Random.error_to_string err))
  in
  request.on_event
    (
      Campaign_started {
        runs = request.runs;
        max_len = effective_max_len request;
        duration_ms = duration_ms request.duration;
        dir = request.case_dir;
      }
    );
  let coverage = Coverage.create () in
  let forkserver = ref None in
  let map_ref = ref None in
  let started_at = Time.Instant.now () in
  let last_progress_ms = ref (-2_000) in
  let cleanup () =
    let _ = stop_forkserver !forkserver in
    let _ = close_map !map_ref in
    ()
  in
  let run_with_tempdir tempdir =
    let input_path = Path.(tempdir / Path.v "input") in
    let* map = Afl.create_map () in
    map_ref := Some map;
    let rec loop corpus run_index =
      if run_index > request.runs || duration_elapsed started_at request.duration then
        Ok {
          runs = run_index - 1;
          crash_path = None;
          total_edges = Coverage.total_edges coverage;
          elapsed_ms = elapsed_millis started_at;
        }
      else
        let* base = Mutation.choose_corpus_input rng corpus in
        let max_len = effective_max_len request in
        let* input =
          Mutation.mutate
            rng
            ~max_len
            ~corpus
            ~dictionary:request.mutator.dictionary
            ~splicing:request.mutator.splicing
            base
        in
        let* () =
          Fs.write input input_path
          |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
        in
        let* (status, snapshot) =
          execute_with_forkserver
            ~target:request.target
            ~input_path
            ~map
            ~forkserver_ref:forkserver
            ~timeout_ms:request.timeout_ms
        in
        let novelty = Coverage.record coverage snapshot in
        let elapsed_ms = elapsed_millis started_at in
        if should_emit_progress ~last_progress_ms ~run_index ~elapsed_ms then (
          last_progress_ms := elapsed_ms;
          request.on_event
            (
              Campaign_progress {
                run = run_index;
                runs = request.runs;
                elapsed_ms;
                total_edges = Coverage.total_edges coverage;
                corpus_size = List.length corpus;
              }
            )
        );
      request.on_event
        (
          Input_executed {
            run = run_index;
            status;
            hit_edges = novelty.hit_edges;
            new_edges = novelty.new_edges;
          }
        );
      if status_failed status then
        let* crash_path = Corpus.save_input crashes_dir "crash-" input in
        request.on_event (Crash_found { run = run_index; path = crash_path; status });
        let* () = triage_crash request ~run:run_index ~input_path:crash_path ~status in
        Ok {
          runs = run_index;
          crash_path = Some crash_path;
          total_edges = Coverage.total_edges coverage;
          elapsed_ms = elapsed_millis started_at;
        }
      else if novelty.new_edges > 0 then
        let* corpus_path = Corpus.save_input corpus_dir "" input in
        request.on_event
          (Corpus_saved { run = run_index; path = corpus_path; new_edges = novelty.new_edges });
        loop (input :: corpus) (run_index + 1)
      else
        loop corpus (run_index + 1)
    in
    loop corpus 1
  in
  let result =
    try
      match Fs.with_tempdir ~prefix:"riot-fuzz-" run_with_tempdir with
      | Error err -> Error (Error.Io_error (IO.error_message err))
      | Ok (Error err) -> Error err
      | Ok (Ok result) -> Ok result
    with
    | exn -> Error (Error.Runtime_error (Exception.to_string exn))
  in
  cleanup ();
  match result with
  | Error _ as err -> err
  | Ok result ->
      request.on_event
        (
          Campaign_completed {
            runs = result.runs;
            crash_path = result.crash_path;
            total_edges = result.total_edges;
            elapsed_ms = result.elapsed_ms;
          }
        );
      Ok result

let replay = fun ~target ~input_path ~timeout_ms ->
  let coverage = Coverage.create () in
  let forkserver = ref None in
  let map_ref = ref None in
  let cleanup () =
    let _ = stop_forkserver !forkserver in
    let _ = close_map !map_ref in
    ()
  in
  let* map = Afl.create_map () in
  map_ref := Some map;
  let result =
    try
      execute_with_forkserver ~target ~input_path ~map ~forkserver_ref:forkserver ~timeout_ms
      |> Result.map
        ~fn:(fun (status, snapshot) ->
          let novelty = Coverage.record coverage snapshot in
          { input_path; status; hit_edges = novelty.hit_edges })
    with
    | exn -> Error (Error.Runtime_error (Exception.to_string exn))
  in
  cleanup ();
  result

let minimize_corpus = fun (request: minimize_request) ->
  let corpus_dir = Path.(request.case_dir / Path.v "corpus") in
  let entries =
    Corpus.load_entries corpus_dir
    |> List.filter
      ~fn:(fun entry -> not (String.equal (Path.basename entry.Corpus.path) "seed-empty"))
    |> List.sort
      ~compare:(fun left right ->
        let by_len =
          Int.compare (String.length left.Corpus.content) (String.length right.Corpus.content)
        in
        match by_len with
        | Order.EQ -> Corpus.compare_path left.path right.path
        | Order.LT
        | Order.GT -> by_len)
  in
  let coverage = Coverage.create () in
  let forkserver = ref None in
  let map_ref = ref None in
  let cleanup () =
    let _ = stop_forkserver !forkserver in
    let _ = close_map !map_ref in
    ()
  in
  let run_with_tempdir tempdir =
    let input_path = Path.(tempdir / Path.v "input") in
    let* map = Afl.create_map () in
    map_ref := Some map;
    let rec loop kept removed = fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok (kept, removed)
      | entry :: rest ->
          let* () =
            Fs.write entry.Corpus.content input_path
            |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
          in
          let* (status, snapshot) =
            execute_with_forkserver
              ~target:request.target
              ~input_path
              ~map
              ~forkserver_ref:forkserver
              ~timeout_ms:request.timeout_ms
          in
          let novelty = Coverage.record coverage snapshot in
          if status_failed status || novelty.new_edges > 0 then
            loop (kept + 1) removed rest
          else
            let* () = Corpus.delete_input entry.path in
            loop kept (removed + 1) rest
    in
    loop 0 0 entries
  in
  let result =
    try
      match Fs.with_tempdir ~prefix:"riot-fuzz-minimize-" run_with_tempdir with
      | Error err -> Error (Error.Io_error (IO.error_message err))
      | Ok (Error err) -> Error err
      | Ok (Ok (kept, removed)) -> Ok { dir = corpus_dir; kept; removed }
    with
    | exn -> Error (Error.Runtime_error (Exception.to_string exn))
  in
  cleanup ();
  match result with
  | Error _ as err -> err
  | Ok result ->
      request.on_event
        (Corpus_minimized { dir = result.dir; kept = result.kept; removed = result.removed });
      Ok result

type campaign_task = {
  index: int;
  request: request;
}

type Message.t +=
  | Fuzz_worker_event of {
      run_ref: unit Ref.t;
      index: int;
      event: event;
    }
  | Fuzz_worker_completed of {
      run_ref: unit Ref.t;
      index: int;
      result: (result, Error.t) Result.t;
    }

type pool_event =
  | Worker_ready of campaign_task WorkerPool.DynamicWorkerPool.worker
  | Worker_event of int * event
  | Worker_completed of int * (result, Error.t) Result.t

let cast_worker:
  type task other. (task, other) Type.eq ->
  other WorkerPool.DynamicWorkerPool.worker ->
  task WorkerPool.DynamicWorkerPool.worker = fun witness worker ->
  match witness with
  | Type.Equal -> worker

let run_many = fun ?concurrency requests ->
  let concurrency =
    match concurrency with
    | Some concurrency -> Int.max 1 concurrency
    | None -> Int.max 1 Thread.available_parallelism
  in
  if List.is_empty requests then
    { campaigns = [] }
  else
    let owner = self () in
    let run_ref = Ref.make () in
    let pending = Queue.create () in
    requests
    |> List.enumerate
    |> List.for_each ~fn:(fun (index, request) -> Queue.push pending ~value:{ index; request });
  let worker_fn ~owner ~task =
    let request = {
      task.request with
      on_event = (fun event -> send owner (Fuzz_worker_event { run_ref; index = task.index; event }));
    }
    in
    let result =
      try run request with
      | exn -> Error (Error.Runtime_error (Exception.to_string exn))
    in
    send owner (Fuzz_worker_completed { run_ref; index = task.index; result })
  in
  let pool = WorkerPool.DynamicWorkerPool.start ~concurrency ~owner ~worker_fn () in
  let dispatch_event index event =
    match List.get requests ~at:index with
    | Some request -> request.on_event event
    | None -> ()
  in
  let rec loop finished_count results_rev =
    if Int.equal finished_count (List.length requests) then
      results_rev
    else
      let selector: pool_event selector = fun __tmp1 ->
        match __tmp1 with
        | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
            match Ref.type_equal
              pool.task_ref
              (WorkerPool.DynamicWorkerPool.get_worker_task_ref worker) with
            | Some witness -> Select (Worker_ready (cast_worker witness worker))
            | None -> Skip
          )
        | Fuzz_worker_event { run_ref = ref; index; event } when Ref.equal run_ref ref ->
            Select (Worker_event (index, event))
        | Fuzz_worker_completed { run_ref = ref; index; result } when Ref.equal run_ref ref ->
            Select (Worker_completed (index, result))
        | _ -> Skip
      in
      match receive ~selector () with
      | Worker_ready worker -> (
          match Queue.pop pending with
          | Some task ->
              WorkerPool.DynamicWorkerPool.send_task pool worker task;
              loop finished_count results_rev
          | None -> loop finished_count results_rev
        )
      | Worker_event (index, event) ->
          dispatch_event index event;
          loop finished_count results_rev
      | Worker_completed (index, result) ->
          loop (finished_count + 1) (({ index; result }: campaign_result) :: results_rev)
  in
  let campaigns =
    loop 0 []
    |> List.sort
      ~compare:(fun (left: campaign_result) (right: campaign_result) ->
        Int.compare
          left.index
          right.index)
  in
  { campaigns }

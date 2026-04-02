open Std

let tests =
  Test.FixtureRunner.cases
    ~dir:"packages/std/tests/fixtures/snapshot_fixture_runner"
    ~run:(fun ctx ->
      let actual = Fs.read ctx.fixture_path |> Result.expect ~msg:"read fixture" in
      Test.Snapshot.assert_text ~ctx:ctx.test ~actual)

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"std_fixture_runner_tests" ~tests ~args)
    ~args:Env.args
    ()

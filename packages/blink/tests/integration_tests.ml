open Std
module Test = Std.Test

let tests = []

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"blink_integration" ~tests ~args)
    ~args:Env.args
    ()

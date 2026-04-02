module Runner = Runner
module Assertions = Assertions
module Cli = Cli
module Context = struct
  include Test_context
end
module Snapshot = Snapshot
module FixtureRunner = Fixture_runner

type test_result = Test_case.test_result
type ctx = Test_context.t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: string option;
  binary_path: string option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: Test_context.fixture option;
}

type test_type =
  | UnitTest
  | Property of { examples: int }

type test_case = Test_case.t

let case = Test_case.case

let property = Test_case.property

let skip = Test_case.skip

let todo = Test_case.todo

include Assertions

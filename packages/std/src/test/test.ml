module Runner = Runner
module Assertions = Assertions
module Cli = Cli

module Context = struct
  include Test_context
end

module Snapshot = Snapshot
module FixtureRunner = Fixture_runner
module Fuzz = Fuzz

type test_result = Test_case.test_result

type ctx = Test_context.t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  built_binaries: Test_context.built_binary list;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: Test_context.fixture option;
  progress_handler: Test_context.progress_handler;
}

type test_type =
  | UnitTest
  | Property of { examples: int }
  | Fuzz of { seeds: int }

type size = Test_case.size =
  | Small
  | Large

type reliability = Test_case.reliability =
  | Stable
  | Flaky of { retry_attempts: int }

type test_case = Test_case.t

let case = Test_case.case

let property = Test_case.property

let fuzz = Test_case.fuzz

let skip = Test_case.skip

let todo = Test_case.todo

include Assertions

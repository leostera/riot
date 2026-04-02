open Std
module Test = Std.Test

let assert_roundtrip = fun action ->
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok decoded ->
      if Tusk_planner.Action.equal action decoded then
        Ok ()
      else
        Error "action changed after json roundtrip"
  | Error err -> Error ("failed to decode action json: " ^ err)

let compile_interface_roundtrip_preserves_fields = fun _ctx ->
  assert_roundtrip
    (Tusk_planner.Action.CompileInterface {
      source = Path.v "src/a.mli";
      outputs = [ Path.v "a.cmi" ];
      includes = [ Path.v "src"; Path.v "vendor/lib" ];
      flags = [ Tusk_toolchain.Ocamlc.Open "Std"; Tusk_toolchain.Ocamlc.NoAliasDeps ]
    })

let compile_implementation_roundtrip_preserves_flags = fun _ctx ->
  assert_roundtrip
    (Tusk_planner.Action.CompileImplementation {
      source = Path.v "src/a.ml";
      outputs = [ Path.v "a.cmx"; Path.v "a.o" ];
      includes = [ Path.v "src" ];
      flags = [
        Tusk_toolchain.Ocamlc.Open "Std";
        Tusk_toolchain.Ocamlc.NoStdlib;
        Tusk_toolchain.Ocamlc.NoPervasives;
      ]
    })

let compile_c_roundtrip_preserves_ccflags = fun _ctx ->
  assert_roundtrip
    (Tusk_planner.Action.CompileC {
      source = Path.v "native/stub.c";
      outputs = [ Path.v "stub.o" ];
      ccflags = [ "-O3"; "-fPIC"; "-DTEST=1" ]
    })

let create_executable_roundtrip_preserves_linker_fields = fun _ctx ->
  assert_roundtrip
    (
      Tusk_planner.Action.CreateExecutable {
        outputs = [ Path.v "bin/app.exe" ];
        objects = [ Path.v "a.cmx"; Path.v "b.o" ];
        libraries = [ Path.v "libfoo.cmxa"; Path.v "libbar.cma" ];
        includes = [ Path.v "src"; Path.v "_build/deps" ];
        cclibs = [ Path.v "libfoo.a"; Path.v "libbar.a" ];
        ccopt_flags = [ "-Wl,-rpath,/tmp/lib"; "-pthread" ];
        cclib_flags = [ "-lssl"; "-lcrypto" ];
      }
    )

let create_shared_library_roundtrip_preserves_linker_fields = fun _ctx ->
  assert_roundtrip
    (
      Tusk_planner.Action.CreateSharedLibrary {
        outputs = [ Path.v "lib/app.cmxs" ];
        objects = [ Path.v "entry.cmx"; Path.v "ffi.o" ];
        libraries = [ Path.v "runtime.cmxa" ];
        includes = [ Path.v "src"; Path.v "runtime" ];
        cclibs = [ Path.v "libruntime.a" ];
        ccopt_flags = [ "-fPIC" ];
        cclib_flags = [ "-ldl"; "-lm" ];
      }
    )

let build_foreign_dependency_roundtrip_preserves_env_and_outputs = fun _ctx ->
  assert_roundtrip
    (
      Tusk_planner.Action.BuildForeignDependency {
        name = "ring";
        path = Path.v "native/ring";
        build_cmd = [ "cargo"; "build"; "--release" ];
        outputs = [ Path.v "target/release/libring.a"; Path.v "target/release/ring.h" ];
        env = [ ("RUSTFLAGS", "-C target-cpu=native"); ("CC", "clang") ];
      }
    )

let compile_implementation_roundtrip_preserves_combined_warning_flags = fun _ctx ->
  assert_roundtrip
    (Tusk_planner.Action.CompileImplementation {
      source = Path.v "src/warn.ml";
      outputs = [ Path.v "warn.cmx"; Path.v "warn.o" ];
      includes = [ Path.v "src" ];
      flags = [
        Tusk_toolchain.Ocamlc.Warning [ Tusk_toolchain.Ocamlc.All; Tusk_toolchain.Ocamlc.NoCmiFile; ];
      ]
    })

let compile_implementation_roundtrip_preserves_profile_style_flags = fun _ctx ->
  assert_roundtrip
    (Tusk_planner.Action.CompileImplementation {
      source = Path.v "src/release.ml";
      outputs = [ Path.v "release.cmx"; Path.v "release.o" ];
      includes = [ Path.v "src" ];
      flags = [
        Tusk_toolchain.Ocamlc.Inline 100;
        Tusk_toolchain.Ocamlc.NoAssert;
        Tusk_toolchain.Ocamlc.Compact;
        Tusk_toolchain.Ocamlc.WarnError [ Tusk_toolchain.Ocamlc.All ];
        Tusk_toolchain.Ocamlc.Raw "-O2";
      ]
    })

let tests =
  Test.[
    case "compile interface json roundtrip" compile_interface_roundtrip_preserves_fields;
    case "compile implementation json roundtrip" compile_implementation_roundtrip_preserves_flags;
    case "compile c json roundtrip" compile_c_roundtrip_preserves_ccflags;
    case "create executable json roundtrip" create_executable_roundtrip_preserves_linker_fields;
    case "create shared library json roundtrip" create_shared_library_roundtrip_preserves_linker_fields;
    case "build foreign dependency json roundtrip" build_foreign_dependency_roundtrip_preserves_env_and_outputs;
    case "compile implementation combined warning flags roundtrip" compile_implementation_roundtrip_preserves_combined_warning_flags;
    case "compile implementation profile-style flags roundtrip" compile_implementation_roundtrip_preserves_profile_style_flags;
  ]

let name = "tusk-planner:action-json-roundtrip"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

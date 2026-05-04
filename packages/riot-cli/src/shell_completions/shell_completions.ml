open Std

type shell =
  | Zsh
  | Bash
  | Fish

let shell_to_string = fun __tmp1 ->
  match __tmp1 with
  | Zsh -> "zsh"
  | Bash -> "bash"
  | Fish -> "fish"

let shell_from_string = fun __tmp1 ->
  match __tmp1 with
  | "zsh" -> Some Zsh
  | "bash" -> Some Bash
  | "fish" -> Some Fish
  | _ -> None

let list_packages = fun (workspace: Riot_model.Workspace_manifest.t) ->
  workspace.packages
  |> List.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)
  |> List.sort ~compare:Riot_model.Package_name.compare
  |> List.map ~fn:Riot_model.Package_name.to_string

let realized_workspace_packages = fun ~intent (workspace: Riot_model.Workspace_manifest.t) ->
  Riot_model.Workspace_manifest.realize_packages ~intent workspace
  |> List.filter ~fn:Riot_model.Package.is_workspace_member

(** List binaries as "package:binary" for display in completions, excluding tests *)
let list_binaries = fun (workspace: Riot_model.Workspace_manifest.t) ->
  realized_workspace_packages ~intent:Riot_model.Package.Run workspace
  |> List.flat_map
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      List.filter_map
        pkg.binaries
        ~fn:(fun (bin: Riot_model.Package.binary) ->
          (* Filter out test binaries *)
          if
            String.ends_with ~suffix:"_tests" bin.name || String.ends_with ~suffix:"-tests" bin.name
          then
            None
          else
            Some (Riot_model.Package_name.to_string pkg.name ^ ":" ^ bin.name)))
  |> List.unique ~compare:String.compare

(** List package names, package wildcards, and test binaries for completions *)
let list_tests = fun (workspace: Riot_model.Workspace_manifest.t) ->
  let test_packages =
    realized_workspace_packages ~intent:Riot_model.Package.Test workspace
    |> List.filter_map
      ~fn:(fun (pkg: Riot_model.Package.t) ->
        let has_tests =
          List.any
            pkg.binaries
            ~fn:(fun (bin: Riot_model.Package.binary) ->
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name)
        in
        if has_tests then
          Some (Riot_model.Package_name.to_string pkg.name)
        else
          None)
  in
  let individual_tests =
    realized_workspace_packages ~intent:Riot_model.Package.Test workspace
    |> List.flat_map
      ~fn:(fun (pkg: Riot_model.Package.t) ->
        List.filter_map
          pkg.binaries
          ~fn:(fun (bin: Riot_model.Package.binary) ->
            if
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name
            then
              Some (Riot_model.Package_name.to_string pkg.name ^ ":" ^ bin.name)
            else
              None))
  in
  (* Add pkg:... entries for packages with tests *)
  let package_wildcards = List.map test_packages ~fn:(fun pkg_name -> pkg_name ^ ":...") in
  ((test_packages @ package_wildcards) @ individual_tests)
  |> List.unique ~compare:String.compare

(** List benchmark binaries as "package:bench" for display in completions *)
let list_benchmarks = fun (workspace: Riot_model.Workspace_manifest.t) ->
  let individual_benches =
    realized_workspace_packages ~intent:Riot_model.Package.Bench workspace
    |> List.flat_map
      ~fn:(fun (pkg: Riot_model.Package.t) ->
        List.filter_map
          pkg.binaries
          ~fn:(fun (bin: Riot_model.Package.binary) ->
            if String.ends_with ~suffix:"_bench" bin.name then
              Some (Riot_model.Package_name.to_string pkg.name ^ ":" ^ bin.name)
            else
              None))
  in
  (* Add pkg:... entries for packages with benchmarks *)
  let package_wildcards =
    realized_workspace_packages ~intent:Riot_model.Package.Bench workspace
    |> List.filter_map
      ~fn:(fun (pkg: Riot_model.Package.t) ->
        let has_benches =
          List.any
            pkg.binaries
            ~fn:(fun (bin: Riot_model.Package.binary) -> String.ends_with ~suffix:"_bench" bin.name)
        in
        if has_benches then
          Some (Riot_model.Package_name.to_string pkg.name ^ ":...")
        else
          None)
  in
  (package_wildcards @ individual_benches)
  |> List.unique ~compare:String.compare

(** List package commands as "package:command\tdescription" (tab-separated) for display in completions *)
let list_commands = fun (workspace: Riot_model.Workspace_manifest.t) ->
  Riot_model.Workspace_manifest.discover_commands workspace
  |> List.map
    ~fn:(fun (cmd: Riot_model.Package_command.t) ->
      let name = Riot_model.Package_name.to_string cmd.package_name ^ ":" ^ cmd.name in
      (* Use help text from TOML, or provide fallback *)
      let desc =
        if String.length cmd.description = 0 then
          "Package command"
        else
          cmd.description
      in
      let tab = '\t' in
      (* Explicit tab character *)
      name ^ String.make ~len:1 ~char:tab ^ desc)
  |> List.unique ~compare:String.compare

(** List package command descriptions matching the order of list_commands *)
let list_command_descriptions = fun (workspace: Riot_model.Workspace_manifest.t) ->
  list_commands workspace
  |> List.map
    ~fn:(fun line ->
      (* Extract description after tab *)
      let rec find_tab at =
        if at >= String.length line then
          None
        else if Char.equal (String.get_unchecked line ~at) '\t' then
          Some at
        else
          find_tab (at + 1)
      in
      match find_tab 0 with
      | Some idx -> String.sub line ~offset:(idx + 1) ~len:(String.length line - idx - 1)
      | None -> "Package command")

let generate_zsh_script = fun () ->
  {|#compdef riot

_riot() {
    local -a builtin_commands package_commands all_commands

    builtin_commands=(
        'build:Build packages'
        'fix:Lint code and optionally apply safe fixes'
        'add:Add dependencies'
        'rm:Remove dependencies'
        'update:Update locked dependencies'
        'run:Run a binary'
        'test:Run tests'
        'fuzz:Run fuzz campaigns'
        'bench:Run benchmarks'
        'clean:Clean build artifacts'
        'install:Install dependencies'
        'login:Save pkgs.ml API token'
        'logout:Remove saved pkgs.ml API token'
        'yank:Yank a published package version'
        'publish:Publish packages'
        'new:Create new package'
        'search:Search registry packages'
        'info:Show resolved workspace or package information'
        'toolchain:Manage OCaml toolchains'
        'toolchains:Manage OCaml toolchains'
        'completions:Generate shell completions'
        'doc:Generate documentation'
        'docs:Generate documentation'
        'lsp:Start LSP server'
        'version:Show version'
    )

    # Load package commands dynamically (format: "package:command\tdescription")
    local -a raw_lines package_commands package_descs
    raw_lines=(${(f)"$(riot completions --commands 2>/dev/null)"})
    
    # Parse tab-separated name and description
    package_commands=()
    package_descs=()
    for line in $raw_lines; do
        local name="${line%%$'\t'*}"
        local desc="${line#*$'\t'}"
        package_commands+=("$name")
        package_descs+=("$desc")
    done

    local context state state_descr line
    typeset -A opt_args

    # Check if we've passed -- separator, if so use default file completion
    local i
    for i in {2..$CURRENT}; do
        if [[ "${words[$i]}" == "--" ]]; then
            _files
            return 0
        fi
    done

    # If we're completing the first argument (the command), show all commands
    if [[ $CURRENT -eq 2 ]]; then
        # Combine builtin and package commands for _describe
        local -a all_commands_with_descs
        all_commands_with_descs=($builtin_commands)
        
        # Add package commands in "name:description" format
        for i in {1..${#package_commands[@]}}; do
            # Escape colons in the command name for _describe
            local escaped_name="${package_commands[$i]//:/\\:}"
            all_commands_with_descs+=("$escaped_name:${package_descs[$i]}")
        done
        
        # Show all commands with descriptions, sorted by _describe
        _describe -t commands 'command' all_commands_with_descs
        return 0
    fi

    case "$words[2]" in
        run)
            # Check if we're completing the binary name (position 3)
            if [[ $CURRENT -eq 3 ]]; then
                local -a binaries
                binaries=(${(f)"$(riot completions --binaries 2>/dev/null)"})
                compadd -a binaries
            else
                _arguments \
                    '(-p --package)'{-p,--package}'[Run binary from package]:package:->packages' \
                    '--list[List runnable binaries in the current workspace]' \
                    '--json[Emit machine-readable JSON output for --list]' \
                    '--release[Use the release build profile]' \
                    '--update[Refresh a cached remote source before running]'

                case $state in
                    packages)
                        local -a packages
                        packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                        _describe 'package' packages
                        ;;
                esac
            fi
            ;;
        build)
            _arguments \
                '(-x --target)'{-x,--target}'[Build for target architecture]:target:' \
                '--all-targets[Build for all configured targets]' \
                '--json[Emit machine-readable JSONL events]' \
                '*:package:->packages'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        test)
            _arguments \
                '(-p --package)'{-p,--package}'[Run tests from package]:package:->packages' \
                '(-f --filter)'{-f,--filter}'[Filter test suites and cases by substring]:filter:' \
                '--list[List test suites and cases without running them]' \
                '--release[Use the release build profile]' \
                '--json[Emit machine-readable JSONL events]' \
                '--small[Run only tests marked small]' \
                '--large[Run only tests marked large]' \
                '--flaky[Run only tests marked flaky]' \
                '(-v --verbose)'{-v,--verbose}'[Verbose output]'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        fuzz)
            _arguments \
                '1:subcommand:(minimize-corpus)' \
                '(-p --package)'{-p,--package}'[Fuzz cases from package]:package:->packages' \
                '(-f --filter)'{-f,--filter}'[Filter fuzz suites and cases by substring]:filter:' \
                '--list[List fuzz cases without running them]' \
                '--runs[Number of generated inputs to execute]:count:' \
                '--duration[Maximum campaign duration, such as 30s, 10m, or 1h]:duration:' \
                '--max-len[Maximum generated input length]:bytes:' \
                '--seed[Deterministic fuzzer seed]:seed:' \
                '--concurrency[Number of fuzz campaigns to run in parallel]:count:' \
                '--timeout-ms[Maximum time for one generated input]:milliseconds:' \
                '--replay[Replay a saved input against one selected fuzz case]:path:_files' \
                '--minimize-corpus[Deprecated alias for riot fuzz minimize-corpus]' \
                '--json[Emit machine-readable JSONL events]'
            
            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        bench)
            _arguments \
                '(-p --package)'{-p,--package}'[Run benchmarks from package]:package:->packages' \
                '(-f --filter)'{-f,--filter}'[Filter benchmark suites and cases by substring]:filter:' \
                '--compare[Show up to N previous comparable suite runs]:count:' \
                '--iterations[Override iteration count for all matched benchmarks]:count:' \
                '--warmup[Override warmup count for all matched benchmarks]:count:' \
                '--list[List benchmark suites and cases without running them]' \
                '--release[Use the release build profile]' \
                '--json[Emit machine-readable JSONL events]' \
                '(-v --verbose)'{-v,--verbose}'[Verbose output]'
            
            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        search)
            _arguments \
                '--json[Emit machine-readable JSON results]' \
                '(-n --limit)'{-n,--limit}'[Maximum number of results to return]:limit:' \
                ':query:'
            ;;
        add)
            _arguments \
                '(-p --package)'{-p,--package}'[Edit a specific workspace package manifest]:package:->packages' \
                '--workspace[Edit the workspace root manifest]' \
                '--build[Write into build-dependencies]' \
                '--dev[Write into dev-dependencies]' \
                '--json[Emit machine-readable JSONL events]' \
                '*:dependency:'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        rm)
            _arguments \
                '(-p --package)'{-p,--package}'[Edit a specific workspace package manifest]:package:->packages' \
                '--workspace[Edit the workspace root manifest]' \
                '--build[Remove from build-dependencies]' \
                '--dev[Remove from dev-dependencies]' \
                '--json[Emit machine-readable JSONL events]' \
                '*:dependency:'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        update)
            _arguments \
                '--json[Emit machine-readable JSONL events]' \
                '*:package:->packages'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        publish)
            _arguments \
                '(-p --package)'{-p,--package}'[Publish a specific workspace package]:package:->packages' \
                '--workspace[Publish workspace packages in dependency order]' \
                '--dry-run[Run local publish checks without uploading]' \
                '--skip-fmt[Skip the fmt preflight step]' \
                '--skip-check[Skip the fix preflight step]' \
                '--json[Emit machine-readable JSONL events]'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(riot completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        info)
            _arguments \
                '--json[Emit machine-readable JSON output]'
            ;;
        toolchain|toolchains)
            if [[ $CURRENT -eq 3 ]]; then
                compadd list install list-available
            fi
            ;;
        completions)
            if [[ $CURRENT -eq 3 ]]; then
                compadd install
            elif [[ "${words[3]}" == "install" ]]; then
                _arguments \
                    '--shell[Shell type]:shell:(bash zsh fish)'
            else
                _arguments \
                    '--shell[Shell type]:shell:(bash zsh fish)' \
                    '--packages[List packages]' \
                    '--binaries[List binaries]' \
                    '--tests[List tests]' \
                    '--benchmarks[List benchmarks]' \
                    '--commands[List commands]'
            fi
            ;;
        fix)
            _arguments \
                '--apply[Apply safe fixes to files]' \
                '--check[Check for issues without modifying files]' \
                '--json[Emit machine-readable JSON output]' \
                ':path:_files'
            ;;
        fmt)
            _arguments \
                '--check[Check if files need formatting]' \
                '--verify[Verify formatting would preserve syntax hashes]' \
                '--json[Emit machine-readable JSONL events]'
            ;;
        lsp)
            if [[ $CURRENT -eq 3 ]]; then
                compadd stdio
            fi
            ;;
        clean|install|login|logout|new|doc|docs|version)
            # These commands have their own completion logic
            # Can be extended later
            ;;
        *:*)
            # Package command (format: package:command)
            # Just complete files - users can use --help to learn about options
            _files
            ;;
        *)
            _describe 'command' all_commands
            ;;
    esac
}

_riot "$@"
|}

let generate_bash_script = fun () ->
  (* Placeholder for future bash support *)
  "# Bash completions not yet implemented\n"

let generate_fish_script = fun () ->
  (* Placeholder for future fish support *)
  "# Fish completions not yet implemented\n"

let generate_script = fun __tmp1 ->
  match __tmp1 with
  | Zsh -> generate_zsh_script ()
  | Bash -> generate_bash_script ()
  | Fish -> generate_fish_script ()

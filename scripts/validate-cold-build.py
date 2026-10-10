#!/usr/bin/env python3
"""Reproduce cold/warm builds from a committed source export and isolated HOME."""

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import tarfile
import tempfile
import tomllib

SPEC = importlib.util.spec_from_file_location(
    "stability", Path(__file__).with_name("validate-stability.py")
)
stability = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(stability)
ROOT = stability.ROOT


def isolated_environment(home):
    env = os.environ.copy()
    for name in ("RIOT_REGISTRY_DIR", "OCAMLLIB", "CAML_LD_LIBRARY_PATH"):
        env.pop(name, None)
    env["HOME"] = str(home)
    return env


def component(value):
    if not value or value in (".", "..") or any(c in value for c in "/\\\x00"):
        raise argparse.ArgumentTypeError("expected a single path component")
    return value


def copy_toolchain(source, destination):
    # Preserve relative internal links but never let the isolated compiler reach
    # back into the user's installation through a symlink.
    source = source.resolve()
    for path in source.rglob("*"):
        if path.is_symlink():
            resolved = path.resolve(strict=True)
            if not resolved.is_relative_to(source):
                raise ValueError(f"toolchain link escapes its root: {path}")
            if os.path.isabs(os.readlink(path)):
                raise ValueError(f"toolchain link is absolute: {path}")
    shutil.copytree(source, destination, symlinks=True)


def validate_build_output(path):
    events = []
    with path.open() as stream:
        for line in stream:
            if line.strip():
                event = json.loads(line)
                if not isinstance(event, dict):
                    raise ValueError("build stdout must contain JSON objects")
                events.append(event)
    if not events:
        raise ValueError("build emitted no result evidence")
    if any(event.get("type") == "BuildFailed" for event in events):
        raise ValueError("build reported failures")
    final = events[-1]
    if (final.get("type"), final.get("phase")) != ("BuildPhase", "returning_results"):
        raise ValueError("build did not emit a terminal returning_results phase")
    if type(final.get("result_count")) is not int or final["result_count"] <= 0:
        raise ValueError("build returned no package results")
    if final.get("had_partial_failure") is not False:
        raise ValueError("build reported partial failure or omitted failure status")


EDIT_MARKER = "stability-edit-probe"
CLI_ABOUT = "OCaml build system and package manager"


def mutate_cli_about(repo):
    path = repo / "packages/riot-cli/src/cli.ml"
    before = path.read_bytes()
    needle = f'about "{CLI_ABOUT}"'.encode()
    if before.count(needle) != 1:
        raise ValueError("edit probe requires exactly one known CLI description")
    description = EDIT_MARKER.ljust(len(CLI_ABOUT), "-")
    after = before.replace(needle, f'about "{description}"'.encode())
    metadata = path.stat()
    path.write_bytes(after)
    # A content edit must invalidate even when metadata-only fingerprints match.
    os.utime(path, ns=(metadata.st_atime_ns, metadata.st_mtime_ns))
    if path.stat().st_mtime_ns != metadata.st_mtime_ns or len(after) != len(before):
        raise ValueError("edit probe did not preserve source size and mtime")
    return {
        "source_file": str(path),
        "before_sha256": hashlib.sha256(before).hexdigest(),
        "after_sha256": hashlib.sha256(after).hexdigest(),
        "size_bytes": len(before),
        "preserved_mtime_ns": metadata.st_mtime_ns,
        "before_description": CLI_ABOUT,
        "after_description": description,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--driver", required=True, type=Path)
    parser.add_argument("--toolchain", required=True, type=Path, help="installed host compiler directory")
    parser.add_argument("--toolchain-version", required=True, type=component)
    parser.add_argument("--host-target", required=True, type=component)
    parser.add_argument("--revision", default="HEAD", help="committed source revision; uncommitted edits are excluded")
    parser.add_argument("--package", default="kernel", type=component)
    parser.add_argument("--edit-probe", action="store_true", help="riot-cli only: verify a same-size/mtime-preserving source edit through produced CLI help")
    parser.add_argument("--timeout", default=120, type=stability.positive_timeout)
    parser.add_argument("--log-dir", type=Path, help="evidence parent (default: _build/stability)")
    args = parser.parse_args(argv)
    if args.edit_probe and args.package != "riot-cli":
        parser.error("--edit-probe requires --package riot-cli")
    if os.name != "posix":
        parser.error("process-group supervision requires a Unix host")
    driver = args.driver.resolve()
    toolchain = args.toolchain.resolve()
    if not driver.is_file() or not os.access(driver, os.X_OK):
        parser.error(f"driver is not executable: {driver}")
    if not (toolchain / "manifest.json").is_file():
        parser.error("toolchain requires manifest.json for recorded provenance")
    source_sha = subprocess.check_output(
        ["git", "rev-parse", "--verify", f"{args.revision}^{{commit}}"],
        cwd=ROOT, text=True, timeout=10,
    ).strip()
    parent = (args.log_dir or ROOT / "_build/stability").resolve()
    parent.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix="cold-", dir=parent))
    report_path = run / "report.json"
    report = {
        "schema_version": 1,
        "source_sha": source_sha,
        "working_tree_status": stability.git_output("status", "--porcelain"),
        "host": platform.platform(),
        "driver": str(driver),
        "toolchain_source": str(toolchain),
        "toolchain_version": args.toolchain_version,
        "host_target": args.host_target,
        "package": args.package,
        "edit_probe_requested": args.edit_probe,
        "timeout_s": args.timeout,
        "scope": "committed export, isolated HOME and empty build cache; copied compiler and inherited host PATH/native libraries; not a pinned bootstrap certification",
        "passed": False,
        "commands": [],
    }

    def save():
        report_path.write_text(json.dumps(report, indent=2) + "\n")

    save()
    print(f"Evidence: {report_path}", flush=True)
    try:
        archive = run / "source.tar"
        subprocess.run(
            ["git", "archive", "--format=tar", "-o", str(archive), source_sha],
            cwd=ROOT, check=True, timeout=30,
        )
        repo = run / "repo"
        repo.mkdir()
        with tarfile.open(archive) as source:
            source.extractall(repo, filter="data")
        if (repo / "_build").exists():
            raise ValueError("source export unexpectedly contains a build cache")
        home = run / "home"
        home.mkdir()
        compiler = home / ".riot/toolchains" / args.toolchain_version / args.host_target
        copy_toolchain(toolchain, compiler)
        snapshot = run / "riot-driver"
        shutil.copy2(driver, snapshot)
        manifest = json.loads((compiler / "manifest.json").read_text())
        identity = manifest.get("toolchain", {})
        if (identity.get("toolchain_version"), identity.get("host"), identity.get("target")) != (
            args.toolchain_version, args.host_target, args.host_target
        ):
            raise ValueError("copied compiler manifest does not match the requested host/version")
        config = (repo / "ocaml-toolchain.toml").read_text()
        if tomllib.loads(config).get("toolchain", {}).get("version") != args.toolchain_version:
            raise ValueError("exported source toolchain version does not match the copied compiler")
        report.update({
            "repo": str(repo), "home": str(home),
            "driver_snapshot": str(snapshot),
            "driver_sha256": hashlib.sha256(snapshot.read_bytes()).hexdigest(),
            "toolchain_manifest": manifest,
            "toolchain_config": config,
        })
        save()
        env = isolated_environment(home)
        phases = ["cold", "warm"]
        if args.edit_probe:
            phases.extend(["pre-edit-help", "edit", "post-edit-help"])
        produced = repo / "_build/debug" / args.host_target / "out/riot-cli/riot"
        for index, phase in enumerate(phases):
            if phase == "edit":
                report["edit_probe"] = mutate_cli_about(repo)
                save()
            if phase.endswith("help"):
                command = [str(produced), "--help"]
            else:
                command = [str(snapshot), "build", args.package, "--json"]
            print(f"Running {phase}: {' '.join(command)}", flush=True)
            result = stability.run_command(
                command, cwd=repo, env=env, log_dir=run,
                index=index, timeout=args.timeout,
            )
            result["phase"] = phase
            if result["passed"]:
                try:
                    if phase.endswith("help"):
                        output = Path(result["stdout"]).read_text()
                        if phase == "pre-edit-help":
                            if CLI_ABOUT not in output or EDIT_MARKER in output:
                                raise ValueError("baseline produced CLI did not expose the original description")
                            result["executable_sha256"] = hashlib.sha256(produced.read_bytes()).hexdigest()
                        else:
                            if CLI_ABOUT in output or report["edit_probe"]["after_description"] not in output:
                                raise ValueError("produced CLI did not reflect the source edit")
                            result["executable_sha256"] = hashlib.sha256(produced.read_bytes()).hexdigest()
                    else:
                        validate_build_output(Path(result["stdout"]))
                except (OSError, ValueError) as error:
                    result["passed"] = False
                    result["error"] = str(error)
            report["commands"].append(result)
            save()
            if not result["passed"]:
                print(f"FAILED {phase}; no retry or warm-cache substitution: {report_path}", file=sys.stderr)
                return 1
        report["passed"] = True
        save()
        print(f"Required build/probe commands passed: {report_path}")
        return 0
    except (OSError, ValueError, subprocess.SubprocessError, tarfile.TarError) as error:
        report["error"] = str(error)
        save()
        print(f"FAILED preparation: {error}; evidence: {report_path}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

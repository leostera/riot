#!/usr/bin/env python3
"""Check build cleanup exclusion or Ctrl-C recovery in disposable state."""

import argparse
import errno
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

SPEC = importlib.util.spec_from_file_location("cold", Path(__file__).with_name("validate-cold-build.py"))
cold = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cold)
stability = cold.stability


def validate_cleanup_rejection(result, build_root):
    if result["exit_code"] != 1 or result["timed_out"]:
        raise ValueError("active-build cleanup must fail promptly with exit 1")
    events = [json.loads(line) for line in Path(result["stdout"]).read_text().splitlines() if line.strip()]
    if any(not isinstance(event, dict) for event in events):
        raise ValueError("cleanup stdout must contain JSON objects")
    failures = [event for event in events if event.get("type") == "ForceCleanFailed"]
    if len(failures) != 1:
        raise ValueError("cleanup must emit exactly one ForceCleanFailed event")
    failure = failures[0]
    if not isinstance(failure.get("build_root"), str):
        raise ValueError("cleanup failure omitted its build root")
    if Path(failure["build_root"]).resolve() != build_root.resolve():
        raise ValueError("cleanup failure refers to a different build root")
    if not isinstance(failure.get("error"), str) or not failure["error"].startswith("build root is in use by another build or cleanup:"):
        raise ValueError("cleanup failed for a reason other than active-build exclusion")
    if any(event.get("type") in ("ForceCleanStarted", "ForceCleanCompleted") for event in events):
        raise ValueError("cleanup entered its destructive phase while a build was active")


def gate_compiler(compiler, control):
    binary = compiler / "bin/ocamlopt.opt"
    if not binary.is_file() or binary.is_symlink():
        raise ValueError("controlled compiler requires a regular bin/ocamlopt.opt")
    original = binary.with_name("ocamlopt.opt.control-original")
    if original.exists():
        raise ValueError("controlled compiler destination already exists")
    original_digest = hashlib.sha256(binary.read_bytes()).hexdigest()
    binary.rename(original)
    # The first actual compilation owns the gate; version/config queries pass
    # through, and later compilation commands are not blocked a second time.
    quote = shlex.quote
    wrapper = (
        '#!/bin/sh\nfor arg in "$@"; do\n'
        f' if [ "$arg" = "-c" ] && mkdir {quote(str(control / "claim"))} 2>/dev/null; then\n'
        f'  echo "$$" > {quote(str(control / "ready.tmp"))}\n'
        f'  /bin/mv {quote(str(control / "ready.tmp"))} {quote(str(control / "ready"))}\n'
        f'  read release < {quote(str(control / "release"))}\n'
        ' fi\ndone\n'
        f'exec {quote(str(original))} "$@"\n'
    )
    binary.write_text(wrapper)
    binary.chmod(0o755)
    return {"original_sha256": original_digest, "wrapper": wrapper,
            "wrapper_sha256": hashlib.sha256(binary.read_bytes()).hexdigest()}


def wait_for_gate(process, ready, timeout):
    deadline = time.monotonic() + timeout
    while not ready.exists():
        if process.poll() is not None:
            raise ValueError("build exited before controlled compilation started")
        if time.monotonic() >= deadline:
            raise TimeoutError("controlled compilation did not start before the deadline")
        time.sleep(0.05)


def release_gate(process, fifo, timeout):
    deadline = time.monotonic() + timeout
    while True:
        if process.poll() is not None:
            raise ValueError("build exited before the compiler gate was released")
        try:
            fd = os.open(fifo, os.O_WRONLY | os.O_NONBLOCK)
            break
        except OSError as error:
            if error.errno != errno.ENXIO or time.monotonic() >= deadline:
                raise
            time.sleep(0.01)
    try:
        if os.write(fd, b"release\n") != len(b"release\n"):
            raise ValueError("compiler gate release was incomplete")
    finally:
        os.close(fd)


def process_group_survives(process, timeout):
    deadline = time.monotonic() + timeout
    while True:
        process.poll()  # Reap the leader before probing remaining group members.
        try:
            os.killpg(process.pid, 0)
        except ProcessLookupError:
            return False
        if time.monotonic() >= deadline:
            return True
        time.sleep(0.05)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--driver", required=True, type=Path)
    parser.add_argument("--toolchain", required=True, type=Path)
    parser.add_argument("--toolchain-version", required=True, type=cold.component)
    parser.add_argument("--host-target", required=True, type=cold.component)
    parser.add_argument("--timeout", default=120, type=stability.positive_timeout)
    parser.add_argument("--scenario", choices=("cleanup", "cancellation"), default="cleanup")
    parser.add_argument("--log-dir", type=Path)
    parser.add_argument("--fixture-parent", type=Path, default=Path(tempfile.gettempdir()))
    args = parser.parse_args(argv)
    if os.name != "posix":
        parser.error("the compiler gate and process-group supervisor require Unix")
    driver = args.driver.resolve()
    if not driver.is_file() or not os.access(driver, os.X_OK):
        parser.error("driver must be executable")
    fixture_parent = args.fixture_parent.resolve()
    if fixture_parent.is_relative_to(stability.ROOT):
        parser.error("fixture parent must be outside the monorepo to avoid ancestor workspace discovery")
    parent = (args.log_dir or stability.ROOT / "_build/stability").resolve()
    parent.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix="build-cleanup-", dir=parent))
    report_path = run / "report.json"
    report = {
        "schema_version": 1, "passed": False, "commands": [],
        "source_sha": stability.git_output("rev-parse", "HEAD"),
        "working_tree_status": stability.git_output("status", "--porcelain"),
        "harness_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "host": platform.platform(), "host_target": args.host_target,
        "timeout_s": args.timeout, "scenario": args.scenario,
        "scope": "external generated library fixture; private HOME and copied compiler deliberately modified with a FIFO gate; not bootstrap certification or representative-service acceptance",
    }

    def save():
        report_path.write_text(json.dumps(report, indent=2) + "\n")

    save()
    print(f"Evidence: {report_path}", flush=True)
    process = None
    try:
        fixture_parent.mkdir(parents=True, exist_ok=True)
        root = Path(tempfile.mkdtemp(prefix="riot-build-cleanup-", dir=fixture_parent)).resolve()
        repo = root / "repo"
        (repo / "src").mkdir(parents=True)
        (repo / "riot.toml").write_text('[package]\nname = "control"\nversion = "0.0.1"\n[lib]\npath = "src/lib.ml"\n')
        (repo / "ocaml-toolchain.toml").write_text(f'[toolchain]\nversion = {json.dumps(args.toolchain_version)}\n')
        (repo / "src/lib.ml").write_text("let x = 42\n")
        home = root / "home"
        compiler = home / ".riot/toolchains" / args.toolchain_version / args.host_target
        compiler.parent.mkdir(parents=True)
        cold.copy_toolchain(args.toolchain, compiler)
        manifest = json.loads((compiler / "manifest.json").read_text())
        if not isinstance(manifest, dict) or not isinstance(manifest.get("toolchain"), dict):
            raise ValueError("compiler manifest requires a toolchain object")
        identity = manifest["toolchain"]
        if (identity.get("toolchain_version"), identity.get("host"), identity.get("target")) != (
            args.toolchain_version, args.host_target, args.host_target
        ):
            raise ValueError("compiler manifest does not match requested host/version")
        control = root / "control"
        control.mkdir()
        os.mkfifo(control / "release")
        hook = gate_compiler(compiler, control)
        snapshot = run / "riot-driver"
        shutil.copy2(driver, snapshot)
        report.update({"fixture": str(repo), "home": str(home), "compiler_manifest": manifest,
                       "compiler_hook": hook, "driver": str(driver), "driver_snapshot": str(snapshot),
                       "driver_sha256": hashlib.sha256(snapshot.read_bytes()).hexdigest()})
        save()
        env = cold.isolated_environment(home)
        command = [str(snapshot), "build", "control", "--json"]
        build = {"phase": "gated-build", "command": command, "exit_code": None, "passed": False,
                 "timed_out": False, "stdout": str(run / "00.stdout.log"), "stderr": str(run / "00.stderr.log")}
        report["commands"].append(build)
        save()
        with Path(build["stdout"]).open("wb") as out, Path(build["stderr"]).open("wb") as err:
            started = time.monotonic()
            deadline = started + args.timeout

            def remaining(cap=None):
                left = deadline - time.monotonic()
                if left <= 0:
                    raise TimeoutError("gated build exceeded its total command deadline")
                return min(left, cap) if cap is not None else left

            try:
                process = subprocess.Popen(command, cwd=repo, env=env, stdout=out, stderr=err, start_new_session=True)
                wait_for_gate(process, control / "ready", remaining(30))
                sentinel = repo / "_build/cleanup-sentinel"
                sentinel.write_text("active build must survive cleanup\n")
                if args.scenario == "cleanup":
                    clean = stability.run_command([str(snapshot), "clean", "--force", "--json"], cwd=repo,
                        env=env, log_dir=run, index=1, timeout=remaining(10))
                    clean["phase"] = "expected-cleanup-rejection"
                    clean["expectation_passed"] = False
                    report["commands"].append(clean)
                    save()
                    # Retain both the cleanup result and subsequent build result,
                    # even when cleanup violated the exclusion expectation.
                    release_gate(process, control / "release", remaining(10))
                    process.wait(timeout=remaining())
                    build["exit_code"] = process.returncode
                    build["passed"] = process.returncode == 0
                    report["sentinel_survived"] = sentinel.exists()
                    save()
                else:
                    compiler_pid = int((control / "ready").read_text())
                    group = os.getpgid(compiler_pid)
                    report["compiler_pid"] = compiler_pid
                    report["compiler_process_group"] = group
                    report["build_process_group"] = process.pid
                    if group != process.pid:
                        raise ValueError("controlled compiler escaped the supervised build process group")
                    interrupted_at = time.monotonic()
                    os.killpg(process.pid, signal.SIGINT)
                    process.wait(timeout=remaining(10))
                    build["exit_code"] = process.returncode
                    report["cancellation_duration_s"] = round(time.monotonic() - interrupted_at, 3)
                    report["process_group_survived"] = process_group_survives(process, remaining(3))
                    build["expectation_passed"] = process.returncode != 0 and not report["process_group_survived"]
                    save()
                    if not build["expectation_passed"]:
                        raise ValueError("Ctrl-C returned success or left a compiler process behind")
            finally:
                if process is not None:
                    stability.stop_group(process)
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait(timeout=5)
                build["duration_s"] = round(time.monotonic() - started, 3)
                if process is not None:
                    build["exit_code"] = process.returncode
                save()
        if args.scenario == "cleanup":
            validate_cleanup_rejection(clean, repo / "_build")
            clean["expectation_passed"] = True
            if not report["sentinel_survived"] or not build["passed"]:
                raise ValueError("cleanup damaged the active build or its working sentinel")
            cold.validate_build_output(Path(build["stdout"]))
        else:
            # This is recovery after a deliberately interrupted build, not a
            # retry substituting for a failed required gate. The original
            # interrupted command and its expectation remain in the report.
            recovery = stability.run_command(command, cwd=repo, env=env, log_dir=run,
                index=2, timeout=args.timeout)
            recovery["phase"] = "post-cancellation-recovery"
            report["commands"].append(recovery)
            save()
            if not recovery["passed"]:
                raise ValueError("build did not recover after cancellation")
            cold.validate_build_output(Path(recovery["stdout"]))
        report["passed"] = True
        save()
        print(f"Build {args.scenario} scenario passed: {report_path}")
        return 0
    except (OSError, ValueError, TimeoutError, subprocess.SubprocessError) as error:
        report["error"] = str(error)
        if isinstance(error, (TimeoutError, subprocess.TimeoutExpired)) and report["commands"]:
            report["commands"][0]["timed_out"] = True
        save()
        print(f"FAILED: {error}; evidence: {report_path}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""Bounded, local stability checks against an explicitly selected Riot binary."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import signal
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parent.parent
DEFAULT_SUITES = (
    "kernel:kernel_new_foundation_tests",
    "kernel:kernel_new_process_tests",
    "std:std_test_cli_tests",
    "std:std_command_tests",
    "riot-build:build_core_tests",
    "riot-build:action_executor_tests",
    "riot-store:store_tests",
    "riot-cli:test_selection_tests",
    "riot-planner:action_hash_tests",
    "riot-planner:package_planning_tests",
)


def stop_group(process):
    # The leader may already have exited while a child still owns the session.
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        return
    deadline = time.monotonic() + 1
    while time.monotonic() < deadline:
        process.poll()  # Reap the leader, including after SIGTERM.
        try:
            os.killpg(process.pid, 0)
        except ProcessLookupError:
            return
        time.sleep(0.05)
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass


def run_command(command, *, cwd, log_dir, index, timeout, env=None):
    stdout_path = log_dir / f"{index:02d}.stdout.log"
    stderr_path = log_dir / f"{index:02d}.stderr.log"
    result = {
        "command": command,
        "stdout": str(stdout_path),
        "stderr": str(stderr_path),
        "exit_code": None,
        "timed_out": False,
    }
    started = time.monotonic()
    with stdout_path.open("wb") as stdout, stderr_path.open("wb") as stderr:
        try:
            process = subprocess.Popen(
                command, cwd=cwd, env=env, stdout=stdout, stderr=stderr, start_new_session=True
            )
        except OSError as error:
            result["error"] = str(error)
        else:
            try:
                process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                result["timed_out"] = True
            except KeyboardInterrupt:
                result["error"] = "interrupted"
            finally:
                try:
                    stop_group(process)
                except OSError as error:
                    result["cleanup_error"] = str(error)
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    # Keep supervision bounded even if group cleanup was denied.
                    try:
                        process.kill()
                        process.wait(timeout=5)
                    except (OSError, subprocess.TimeoutExpired) as error:
                        result["cleanup_error"] = str(error)
            result["exit_code"] = process.returncode
    result["duration_s"] = round(time.monotonic() - started, 3)
    result["passed"] = (
        result["exit_code"] == 0 and not result["timed_out"]
        and "error" not in result and "cleanup_error" not in result
    )
    return result


def validate_suite_output(path, selector):
    """Require one nonempty successful suite and a consistent terminal summary."""
    package, suite = selector.split(":", 1)
    completed = []
    summaries = []
    last_type = None
    with path.open() as stream:
        for line_number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            try:
                event = json.loads(line)
            except json.JSONDecodeError as error:
                raise ValueError(f"non-JSON stdout at line {line_number}: {error}") from error
            if not isinstance(event, dict):
                raise ValueError(f"expected JSON object at line {line_number}")
            last_type = event.get("type")
            if last_type == "SuiteCompleted":
                completed.append(event)
            elif last_type == "TestSummary":
                summaries.append(event)
    if len(completed) != 1 or len(summaries) != 1 or last_type != "TestSummary":
        raise ValueError("expected one completed suite and one terminal TestSummary")
    event, summary = completed[0], summaries[0]
    if (event.get("package"), event.get("suite")) != (package, suite):
        raise ValueError(f"required suite {selector} did not run")
    if type(event.get("status")) is not int or event["status"] != 0:
        raise ValueError("suite process did not exit successfully")
    counts = event.get("summary")
    if not isinstance(counts, dict):
        raise ValueError("missing suite counts")
    for item in (counts, summary):
        for key in ("total", "passed", "failed", "skipped"):
            if type(item.get(key)) is not int or item[key] < 0:
                raise ValueError(f"invalid {key} count")
        if item["total"] <= 0 or item["passed"] != item["total"]:
            raise ValueError("required suite must contain passing tests")
        if item["failed"] != 0 or item["skipped"] != 0:
            raise ValueError("required suite contains failed or skipped tests")
    if any(counts[key] != summary[key] for key in ("total", "passed", "failed", "skipped")):
        raise ValueError("suite and aggregate counts disagree")
    tests = event.get("tests")
    if not isinstance(tests, list) or len(tests) != counts["total"]:
        raise ValueError("case results do not match suite count")
    if any(not isinstance(case, dict) or case.get("status") != "passed" for case in tests):
        raise ValueError("required suite contains unsuccessful case results")
    if summary.get("failed_tests") != []:
        raise ValueError("aggregate failure list is missing or nonempty")
    return {key: summary[key] for key in ("total", "passed", "failed", "skipped")}


def git_output(*args):
    try:
        return subprocess.check_output(
            ["git", *args], cwd=ROOT, text=True, stderr=subprocess.DEVNULL, timeout=5
        ).strip()
    except (OSError, subprocess.SubprocessError):
        return None


def positive_timeout(value):
    seconds = float(value)
    if not 0 < seconds < float("inf"):
        raise argparse.ArgumentTypeError("timeout must be finite and positive")
    return seconds


def suite_selector(value):
    parts = value.split(":")
    if len(parts) != 2 or not all(parts) or any(part.startswith("-") for part in parts):
        raise argparse.ArgumentTypeError("expected package:suite")
    return value


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--driver", required=True, type=Path, help="explicit Riot executable")
    parser.add_argument("--timeout", type=positive_timeout, default=120, help="seconds per command")
    parser.add_argument("--suite", action="append", type=suite_selector, help="required package:suite; repeatable")
    parser.add_argument("--log-dir", type=Path, help="parent for a unique run directory (default: _build/stability)")
    args = parser.parse_args(argv)
    if os.name != "posix":
        parser.error("process-group supervision currently requires a Unix host")
    driver = args.driver.resolve()
    if not driver.is_file() or not os.access(driver, os.X_OK):
        parser.error(f"driver is not executable: {driver}")
    parent = (args.log_dir or ROOT / "_build" / "stability").resolve()
    parent.mkdir(parents=True, exist_ok=True)
    log_dir = Path(tempfile.mkdtemp(prefix="run-", dir=parent))
    # A build may overwrite the original output path. Keep every invocation on
    # the exact input binary and retain it with the evidence for reproduction.
    snapshot = log_dir / "riot-driver"
    shutil.copy2(driver, snapshot)
    suites = args.suite or list(DEFAULT_SUITES)
    report = {
        "schema_version": 1,
        "source_sha": git_output("rev-parse", "HEAD"),
        "working_tree_status": git_output("status", "--porcelain"),
        "driver": str(driver),
        "driver_snapshot": str(snapshot),
        "driver_sha256": hashlib.sha256(snapshot.read_bytes()).hexdigest(),
        "host": platform.platform(),
        "toolchain_config": (ROOT / "ocaml-toolchain.toml").read_text(),
        "timeout_s": args.timeout,
        "required_suites": suites,
        "scope": "local existing-cache smoke; not clean bootstrap or full certification",
        "passed": False,
        "commands": [],
    }
    report_path = log_dir / "report.json"

    def save():
        report_path.write_text(json.dumps(report, indent=2) + "\n")

    save()
    print(f"Evidence: {report_path}", flush=True)
    commands = [([str(snapshot), "build", "riot-cli", "--json"], None)]
    commands += [([str(snapshot), "test", suite, "--json"], suite) for suite in suites]
    for index, (command, suite) in enumerate(commands):
        print("Running:", " ".join(command), flush=True)
        result = run_command(command, cwd=ROOT, log_dir=log_dir, index=index, timeout=args.timeout)
        if result["passed"] and suite:
            try:
                result["counts"] = validate_suite_output(Path(result["stdout"]), suite)
            except (OSError, ValueError) as error:
                result["passed"] = False
                result["error"] = str(error)
        report["commands"].append(result)
        save()
        if not result["passed"]:
            print(f"FAILED: inspect {report_path}", file=sys.stderr)
            return 1
    report["passed"] = True
    save()
    print(f"Passed {len(suites)} required suites. Evidence: {report_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

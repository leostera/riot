#!/usr/bin/env python3
"""
Legacy helper for non-fixture `riot fix` workflows.

Usage:
  python3 test_runner.py codebase
"""

import argparse
import json
import platform
import shutil
import subprocess
import sys
from pathlib import Path


class TestRunner:
    def __init__(self, workspace_root: Path, timeout_seconds: float = 10.0):
        self.workspace_root = workspace_root
        self.timeout_seconds = timeout_seconds
        self.tests_dir = workspace_root / "packages" / "riot-fix" / "tests"
        self.riot_binary = self.find_riot_binary()

    def find_riot_binary(self) -> Path:
        direct = self.workspace_root / "_build" / "debug"

        machine = platform.machine().lower()
        system = platform.system().lower()
        arch = {
            "arm64": "aarch64",
            "aarch64": "aarch64",
            "x86_64": "x86_64",
            "amd64": "x86_64",
        }.get(machine, machine)
        os_name = {
            "darwin": "apple-darwin",
            "linux": "unknown-linux-gnu",
        }.get(system)

        if os_name is not None:
            candidate = direct / (arch + "-" + os_name) / "out" / "riot-cli" / "riot"
            if candidate.exists():
                return candidate

        candidates = sorted(direct.glob("*/out/riot-cli/riot"))
        if candidates:
            return candidates[0]

        fallback = sorted((self.workspace_root / "_build").glob("**/out/riot-cli/riot"))
        if fallback:
            return fallback[0]

        return self.workspace_root / "_build" / "debug" / "out" / "riot-cli" / "riot"

    def fixture_files(self, pattern: str | None) -> list[Path]:
        files = []
        for path in sorted(self.tests_dir.glob("*.ml")):
            if not path.name[:4].isdigit():
                continue
            if pattern and pattern not in path.name:
                continue
            files.append(path)
        return files

    def run_riot_fix_json(self, target: Path) -> dict | None:
        cmd = [
            str(self.riot_binary),
            "fix",
            "--check",
            "--json",
            str(target),
        ]
        try:
            result = subprocess.run(
                cmd,
                cwd=self.workspace_root,
                capture_output=True,
                text=True,
                timeout=self.timeout_seconds,
            )
        except subprocess.TimeoutExpired:
            return None

        if result.returncode not in (0, 1):
            return None

        events = self.extract_json_events(result.stdout)
        if events is None:
            return None

        return self.aggregate_json_events(events)

    def extract_json_events(self, stdout: str) -> list[dict] | None:
        events: list[dict] = []
        for line in stdout.splitlines():
            text = line.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                return None
            if not isinstance(payload, dict):
                return None
            events.append(payload)
        return events

    def aggregate_json_events(self, events: list[dict]) -> dict | None:
        files: list[dict] = []
        summary: dict | None = None

        for event in events:
            event_type = event.get("type")
            if event_type == "file":
                file_result = dict(event)
                file_result.pop("type", None)
                files.append(file_result)
            elif event_type == "summary":
                summary = dict(event)
                summary.pop("type", None)
                summary.pop("limit_reached", None)
            elif event_type == "start":
                continue
            else:
                return None

        if summary is None:
            return None

        files.sort(key=lambda item: item.get("file", ""))
        return {"summary": summary, "files": files}

    def normalize(self, value):
        if isinstance(value, dict):
            normalized = {}
            for key, item in value.items():
                if key == "file" and isinstance(item, str):
                    normalized[key] = self.relative_path(item)
                else:
                    normalized[key] = self.normalize(item)
            return normalized
        if isinstance(value, list):
            return [self.normalize(item) for item in value]
        return value

    def relative_path(self, value: str) -> str:
        path = Path(value)
        try:
            return str(path.relative_to(self.workspace_root))
        except ValueError:
            return value

    def normalize_json(self, payload: dict) -> str:
        return json.dumps(self.normalize(payload), sort_keys=True, separators=(",", ":"))

    def format_json(self, payload: dict) -> str:
        normalized = self.normalize(payload)
        jq = shutil.which("jq")
        if jq:
            result = subprocess.run(
                [jq, "."],
                input=json.dumps(normalized),
                capture_output=True,
                text=True,
                timeout=5.0,
            )
            if result.returncode == 0:
                return result.stdout.rstrip() + "\n"
        return json.dumps(normalized, indent=2) + "\n"

    def run_fixture(self, fixture: Path, refresh: bool) -> tuple[bool, str]:
        expected = fixture.with_suffix(fixture.suffix + ".expected")
        actual = self.run_riot_fix_json(fixture)
        if actual is None:
            return False, f"{fixture.name} (runner failed)"

        if refresh or not expected.exists():
            expected.write_text(self.format_json(actual))
            return True, ""

        expected_text = expected.read_text()
        try:
            expected_norm = self.normalize_json(json.loads(expected_text))
        except json.JSONDecodeError:
            return False, f"{fixture.name} (invalid expected JSON)"

        actual_norm = self.normalize_json(actual)
        if actual_norm == expected_norm:
            return True, ""

        remaining = actual.get("summary", {}).get("remaining_diagnostics", 0)
        return False, f"{fixture.name} ({remaining} issue(s))"

    def run_fixtures(self, pattern: str | None, refresh: bool) -> int:
        fixtures = self.fixture_files(pattern)
        if not fixtures:
            print("No fixtures found.")
            return 0

        passed = 0
        failed: list[str] = []

        for fixture in fixtures:
            ok, detail = self.run_fixture(fixture, refresh)
            if ok:
                passed += 1
            else:
                failed.append(detail)

        if failed:
            print("\nFailed fixtures:")
            for detail in failed:
                print(f"  ✗ {detail}")

        mode = "refreshed" if refresh else "passed"
        print(f"\nResults: {passed} {mode}, {len(failed)} failed")
        return 1 if failed else 0

    def run_codebase(self) -> int:
        payload = self.run_riot_fix_json(self.workspace_root / "packages")
        if payload is None:
            print("Codebase run failed")
            return 1

        files = payload.get("files", [])
        failing = []
        for file_result in files:
            diagnostics = len(file_result.get("diagnostics", []))
            parse_diagnostics = len(file_result.get("parse_diagnostics", []))
            errors = 1 if file_result.get("error") else 0
            total = diagnostics + parse_diagnostics + errors
            if total > 0:
                failing.append((file_result["file"], total))

        print("\n=========================================")
        print(
            f"Results: {payload['summary']['total_files'] - len(failing)} passed, {len(failing)} failed"
        )
        print("=========================================")
        for file_name, total in failing:
            print(f"✗ {self.relative_path(file_name)} ({total} issue(s))")

        return 1 if failing else 0

    def run_all(self, pattern: str | None, refresh: bool) -> int:
        return self.run_fixtures(pattern, refresh)


def main() -> int:
    parser = argparse.ArgumentParser(description="Legacy riot-fix helper")
    parser.add_argument(
        "command",
        nargs="?",
        default="all",
        choices=["fixtures", "codebase", "all"],
    )
    parser.add_argument("--filter", dest="pattern")
    parser.add_argument("--refresh", action="store_true")
    parser.add_argument("--timeout", type=float, default=10.0)
    args = parser.parse_args()

    runner = TestRunner(Path(__file__).resolve().parents[3], args.timeout)

    if args.command in ("fixtures", "all"):
        print("Fixture coverage moved to the native snapshot suite:")
        print("  riot test riot-fix:fixture_tests")
        return 1

    if args.command == "fixtures":
        return runner.run_fixtures(args.pattern, args.refresh)
    if args.command == "codebase":
        return runner.run_codebase()
    return runner.run_all(args.pattern, args.refresh)


if __name__ == "__main__":
    sys.exit(main())

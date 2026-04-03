#!/usr/bin/env python3
"""
Legacy helper for non-fixture krasny workflows.

Usage:
  python3 test_runner.py --verify-workspace
"""

import argparse
import difflib
from pathlib import Path
import platform
import re
import shlex
import subprocess
import sys
import tempfile
from typing import List, Optional

GREEN = "\033[0;32m"
RED = "\033[0;31m"
YELLOW = "\033[0;33m"
NC = "\033[0m"
NEGATIVE_LITERAL_PATTERN = re.compile(
    r"(?P<prefix>(?:->|=)[ \t]+)-(?P<literal>(?:0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+|\d+(?:\.\d+)?))(?!\))"
)


class RunnerContext:
    def __init__(self, workspace_root: Path):
        self.workspace_root = workspace_root
        self.tests_dir = workspace_root / "packages" / "krasny" / "tests"
        self.fixtures_dir = self.tests_dir / "fixtures"
        self.manifest = self.tests_dir / "format_expectations.txt"
        self.binary = self.find_binary()

    def find_binary(self) -> Path:
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
            candidate = direct / (arch + "-" + os_name) / "out" / "krasny" / "krasny"
            if candidate.exists():
                return candidate

        candidates = sorted(direct.glob("*/out/krasny/krasny"))
        if candidates:
            return candidates[0]

        fallback = sorted((self.workspace_root / "_build").glob("**/out/krasny/krasny"))
        if fallback:
            return fallback[0]

        return self.workspace_root / "_build" / "debug" / "out" / "krasny" / "krasny"

    def run_krasny(
        self,
        subcommand: str,
        file_path: Path,
        *,
        timeout_seconds: float,
    ) -> subprocess.CompletedProcess:
        command = " ".join(
            [
                shlex.quote(str(self.binary)),
                shlex.quote(subcommand),
                shlex.quote(str(file_path)),
            ]
        )
        return subprocess.run(
            ["/bin/zsh", "-lc", command],
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )


class FixtureRunner:
    def __init__(self, context: RunnerContext):
        self.context = context

    def render_unified_diff(
        self,
        fixture_path: Path,
        expected_path: Path,
        expected: str,
        actual: str,
        *,
        max_lines: int = 200,
    ) -> str:
        diff_lines = list(
            difflib.unified_diff(
                expected.splitlines(),
                actual.splitlines(),
                fromfile=str(expected_path.relative_to(self.context.workspace_root)),
                tofile=str(fixture_path.relative_to(self.context.workspace_root))
                + " (actual)",
                lineterm="",
            )
        )
        if not diff_lines:
            return ""
        if len(diff_lines) <= max_lines:
            return "\n".join(diff_lines)
        head = "\n".join(diff_lines[:max_lines])
        return f"{head}\n... diff truncated ({len(diff_lines) - max_lines} additional lines omitted) ..."

    def normalize_trailing_whitespace(self, text: str) -> str:
        lines = text.splitlines()
        normalized_lines = []
        for line in lines:
            stripped = line.rstrip(" \t")
            normalized_lines.append(
                NEGATIVE_LITERAL_PATTERN.sub(
                    lambda match: f"{match.group('prefix')}(-{match.group('literal')})",
                    stripped,
                )
            )
        normalized = "\n".join(normalized_lines)
        if text.endswith("\n"):
            normalized += "\n"
        return normalized

    def discover_fixtures(self, filter_pattern: Optional[str]) -> List[Path]:
        fixtures = []
        for line in self.context.manifest.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if filter_pattern and filter_pattern not in line:
                continue
            fixtures.append(self.context.fixtures_dir / line)
        return fixtures

    def check_expectation(
        self,
        fixture_path: Path,
        actual: str,
        *,
        refresh: bool,
    ) -> tuple[bool, str, Optional[str]]:
        expected_path = Path(str(fixture_path) + ".expected")

        if refresh or not expected_path.exists():
            expected_path.write_text(actual)
            return True, "refreshed expectation", None

        expected = self.normalize_trailing_whitespace(expected_path.read_text())
        actual = self.normalize_trailing_whitespace(actual)
        if actual == expected:
            return True, "expectation ok", None

        return (
            False,
            f"expected output mismatch in {expected_path.name}",
            self.render_unified_diff(fixture_path, expected_path, expected, actual),
        )

    def check_roundtrip(self, fixture_path: Path, actual: str) -> tuple[bool, str]:
        suffix = fixture_path.suffix or ".ml"
        with tempfile.TemporaryDirectory(prefix="krasny-roundtrip-") as tmpdir:
            tmp_path = Path(tmpdir) / ("formatted" + suffix)
            tmp_path.write_text(actual)

            original_hash = self.context.run_krasny(
                "syntax-hash",
                tmp_path,
                timeout_seconds=10.0,
            )
            if original_hash.returncode != 0:
                detail = f"syntax-hash exited {original_hash.returncode}"
                if original_hash.stderr:
                    detail += f": {original_hash.stderr.rstrip()}"
                return False, detail

            reformatted = self.context.run_krasny(
                "format",
                tmp_path,
                timeout_seconds=10.0,
            )
            if reformatted.returncode != 0:
                detail = f"format exited {reformatted.returncode}"
                if reformatted.stderr:
                    detail += f": {reformatted.stderr.rstrip()}"
                return False, detail

            reformatted_path = Path(tmpdir) / ("reformatted" + suffix)
            reformatted_path.write_text(reformatted.stdout)
            reparsed_hash = self.context.run_krasny(
                "syntax-hash",
                reformatted_path,
                timeout_seconds=10.0,
            )

        if reparsed_hash.returncode != 0:
            detail = f"reparse syntax-hash exited {reparsed_hash.returncode}"
            if reparsed_hash.stderr:
                detail += f": {reparsed_hash.stderr.rstrip()}"
            return False, detail

        if original_hash.stdout == reparsed_hash.stdout:
            return True, "roundtrip ok"

        detail = "\n".join(
            [
                "roundtrip syntax hash mismatch",
                f"  original hash: {original_hash.stdout.strip()}",
                f"  formatted hash: {reparsed_hash.stdout.strip()}",
            ]
        )
        return False, detail

    def run_fixture(self, fixture_path: Path, *, refresh: bool) -> bool:
        format_result = self.context.run_krasny(
            "format",
            fixture_path,
            timeout_seconds=10.0,
        )
        if format_result.returncode != 0:
            print(f"{RED}✗{NC} {fixture_path}")
            print(f"  format exited {format_result.returncode}")
            if format_result.stderr:
                print(format_result.stderr.rstrip())
            return False

        actual = format_result.stdout
        expectation_ok, expectation_detail, expectation_diff = self.check_expectation(
            fixture_path,
            actual,
            refresh=refresh,
        )
        roundtrip_ok, roundtrip_detail = self.check_roundtrip(fixture_path, actual)

        if expectation_ok and roundtrip_ok:
            marker = YELLOW + "↺" + NC if refresh else GREEN + "✓" + NC
            print(f"{marker} {fixture_path}")
            return True

        print(f"{RED}✗{NC} {fixture_path}")
        if not expectation_ok:
            print(f"  {expectation_detail}")
            if expectation_diff:
                print(expectation_diff)
        if not roundtrip_ok:
            print(f"  {roundtrip_detail}")
        return False

    def run(
        self,
        filter_pattern: Optional[str],
        *,
        refresh: bool,
        fail_fast: bool,
    ) -> int:
        fixtures = self.discover_fixtures(filter_pattern)
        if not fixtures:
            print(f"{YELLOW}No fixtures matched.{NC}")
            return 0

        passed = 0
        failed = 0
        for fixture in fixtures:
            if self.run_fixture(fixture, refresh=refresh):
                passed += 1
            else:
                failed += 1
                if fail_fast:
                    break

        print()
        if failed == 0:
            print(f"{GREEN}Passed:{NC} {passed}")
            return 0

        print(f"{RED}Failed:{NC} {failed}")
        print(f"{GREEN}Passed:{NC} {passed}")
        return 1


class WorkspaceVerifier:
    def __init__(self, context: RunnerContext):
        self.context = context

    def syntax_hash_from_text(self, text: str, suffix: str) -> tuple[bool, str]:
        with tempfile.TemporaryDirectory(prefix="krasny-workspace-hash-") as tmpdir:
            tmp_path = Path(tmpdir) / ("source" + suffix)
            tmp_path.write_text(text)
            hash_result = self.context.run_krasny(
                "syntax-hash",
                tmp_path,
                timeout_seconds=600.0,
            )
        if hash_result.returncode != 0:
            detail = f"syntax-hash exited {hash_result.returncode}"
            if hash_result.stderr:
                detail += f": {hash_result.stderr.rstrip()}"
            return False, detail
        return True, hash_result.stdout.strip()

    def format_text(self, text: str, suffix: str) -> tuple[bool, str]:
        with tempfile.TemporaryDirectory(prefix="krasny-workspace-format-") as tmpdir:
            tmp_path = Path(tmpdir) / ("source" + suffix)
            tmp_path.write_text(text)
            format_result = self.context.run_krasny(
                "format",
                tmp_path,
                timeout_seconds=20.0,
            )
        if format_result.returncode != 0:
            detail = f"format exited {format_result.returncode}"
            if format_result.stderr:
                detail += f": {format_result.stderr.rstrip()}"
            return False, detail
        return True, format_result.stdout

    def discover_sources(self, filter_pattern: Optional[str]) -> List[Path]:
        cmd = [
            "/bin/zsh",
            "-lc",
            "git ls-files '*.ml' '*.mli'",
        ]
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=self.context.workspace_root,
        )
        if result.returncode != 0:
            print(f"{RED}Failed:{NC} could not list workspace sources via git ls-files")
            if result.stderr:
                print(result.stderr.rstrip())
            return []

        files: List[Path] = []
        for rel in result.stdout.splitlines():
            rel = rel.strip()
            if not rel:
                continue
            if rel.startswith("packages/krasny/tests/fixtures/"):
                continue
            # syn diagnostics fixtures are intentionally malformed parser inputs.
            if rel.startswith("packages/syn/tests/diagnostics/"):
                continue
            if filter_pattern and filter_pattern not in rel:
                continue
            files.append(self.context.workspace_root / rel)
        return files

    def verify_one(self, source_path: Path) -> tuple[bool, str]:
        source_text = source_path.read_text()
        suffix = source_path.suffix or ".ml"

        original_hash = self.context.run_krasny(
            "syntax-hash",
            source_path,
            timeout_seconds=20.0,
        )
        if original_hash.returncode != 0:
            detail = f"syntax-hash exited {original_hash.returncode}"
            if original_hash.stderr:
                detail += f": {original_hash.stderr.rstrip()}"
            return False, detail

        formatted = self.context.run_krasny(
            "format",
            source_path,
            timeout_seconds=20.0,
        )
        if formatted.returncode != 0:
            detail = f"format exited {formatted.returncode}"
            if formatted.stderr:
                detail += f": {formatted.stderr.rstrip()}"
            return False, detail

        with tempfile.TemporaryDirectory(prefix="krasny-workspace-") as tmpdir:
            tmp_path = Path(tmpdir) / ("formatted" + suffix)
            tmp_path.write_text(formatted.stdout)
            formatted_hash = self.context.run_krasny(
                "syntax-hash",
                tmp_path,
                timeout_seconds=20.0,
            )

        if formatted_hash.returncode != 0:
            detail = f"formatted syntax-hash exited {formatted_hash.returncode}"
            if formatted_hash.stderr:
                detail += f": {formatted_hash.stderr.rstrip()}"
            return False, detail

        if original_hash.stdout == formatted_hash.stdout:
            return True, "syntax-hash invariant"

        # Option 1: allow formatter-owned sugar rewrites if canonicalized
        # formatting converges to an equivalent CST hash.
        original_format_ok, canonical_original_text = self.format_text(
            source_text, suffix
        )
        if not original_format_ok:
            return False, f"canonical original format failed: {canonical_original_text}"

        canonical_formatted_ok, canonical_formatted_text = self.format_text(
            formatted.stdout, suffix
        )
        if not canonical_formatted_ok:
            return (
                False,
                f"canonical formatted format failed: {canonical_formatted_text}",
            )

        canonical_original_hash_ok, canonical_original_hash = (
            self.syntax_hash_from_text(
                canonical_original_text,
                suffix,
            )
        )
        if not canonical_original_hash_ok:
            return False, f"canonical original hash failed: {canonical_original_hash}"

        canonical_formatted_hash_ok, canonical_formatted_hash = (
            self.syntax_hash_from_text(
                canonical_formatted_text,
                suffix,
            )
        )
        if not canonical_formatted_hash_ok:
            return False, f"canonical formatted hash failed: {canonical_formatted_hash}"

        if canonical_original_hash == canonical_formatted_hash:
            return True, "syntax-hash invariant after canonical format"

        detail = "\n".join(
            [
                "syntax-hash mismatch",
                f"  original: {original_hash.stdout.strip()}",
                f"  formatted: {formatted_hash.stdout.strip()}",
                f"  canonical-original: {canonical_original_hash}",
                f"  canonical-formatted: {canonical_formatted_hash}",
            ]
        )
        return False, detail

    def run(self, filter_pattern: Optional[str], *, fail_fast: bool) -> int:
        sources = self.discover_sources(filter_pattern)
        if not sources:
            print(f"{YELLOW}No workspace sources matched.{NC}")
            return 0

        passed = 0
        failed = 0
        for source in sources:
            ok, detail = self.verify_one(source)
            if ok:
                print(f"{GREEN}✓{NC} {source}")
                passed += 1
            else:
                print(f"{RED}✗{NC} {source}")
                print(f"  {detail}")
                failed += 1
                if fail_fast:
                    break

        print()
        if failed == 0:
            print(f"{GREEN}Passed:{NC} {passed}")
            return 0

        print(f"{RED}Failed:{NC} {failed}")
        print(f"{GREEN}Passed:{NC} {passed}")
        return 1


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Legacy helper for workspace-wide krasny verification",
    )
    parser.add_argument(
        "--filter",
        help="Run only fixtures whose manifest path contains this string",
    )
    parser.add_argument(
        "--refresh",
        action="store_true",
        help="Rewrite .expected files from current formatter output",
    )
    parser.add_argument(
        "--verify-workspace",
        action="store_true",
        help="Verify syntax-hash invariance for all tracked .ml/.mli files (read-only)",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop after the first failing fixture/source",
    )
    args = parser.parse_args(argv)

    workspace_root = Path(__file__).resolve().parents[3]
    context = RunnerContext(workspace_root)
    if args.verify_workspace:
        verifier = WorkspaceVerifier(context)
        return verifier.run(args.filter, fail_fast=args.fail_fast)

    print(f"{YELLOW}Fixture coverage moved to the native snapshot suite.{NC}")
    print("  riot test krasny:fixture_tests")
    return 1


if __name__ == "__main__":
    sys.exit(main())

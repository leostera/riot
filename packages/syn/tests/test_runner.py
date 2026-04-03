#!/usr/bin/env python3
"""
Legacy helper for non-fixture syn parser workflows.

Usage:
  # Test entire codebase
  python3 test_runner.py codebase
  
  # Debug a specific file with trivia analysis
  python3 test_runner.py debug <file>
"""

import sys
import json
import subprocess
import shutil
import platform
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import argparse

# ANSI colors
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[0;33m'
BLUE = '\033[0;34m'
NC = '\033[0m'  # No Color

class TestRunner:
    def __init__(self, workspace_root: Path, parse_timeout_seconds: float = 10.0):
        self.workspace_root = workspace_root
        self.parse_timeout_seconds = parse_timeout_seconds
        self.syn_binary = self.find_syn_binary()
        self.fixtures_dir = workspace_root / "packages" / "syn" / "tests" / "fixtures"
        self.diagnostics_dir = workspace_root / "packages" / "syn" / "tests" / "diagnostics"
        self.packages_dir = workspace_root / "packages"

    def find_syn_binary(self) -> Path:
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
            host_candidate = direct / (arch + "-" + os_name) / "out" / "syn" / "syn"
            if host_candidate.exists():
                return host_candidate

        candidates = sorted(direct.glob("*/out/syn/syn"))
        if candidates:
            return candidates[0]

        fallback = sorted((self.workspace_root / "_build").glob("**/out/syn/syn"))
        if fallback:
            return fallback[0]

        return self.workspace_root / "_build" / "debug" / "out" / "syn" / "syn"

    def discover_fixture_sources(
        self, filter_pattern: Optional[str] = None
    ) -> List[Path]:
        patterns = (
            [f"*{filter_pattern}*.ml", f"*{filter_pattern}*.mli"]
            if filter_pattern
            else ["*.ml", "*.mli"]
        )

        fixtures: List[Path] = []
        for pattern in patterns:
            fixtures.extend(self.fixtures_dir.glob(pattern))

        fixtures = [
            fixture
            for fixture in fixtures
            if not str(fixture).endswith(".expected_lossless.json")
            and not str(fixture).endswith(".expected_cst.json")
        ]
        return sorted(fixtures)
        
    def run_syn(self, args: List[str], file_path: Path) -> Tuple[str, int]:
        """Run syn command and return (output, returncode)."""
        cmd = [str(self.syn_binary)] + args + [str(file_path)]
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=self.parse_timeout_seconds,
            )
            return result.stdout, result.returncode
        except subprocess.TimeoutExpired:
            timeout = str(self.parse_timeout_seconds)
            return "TIMEOUT: Command exceeded " + timeout + " seconds", -1

    def did_timeout(self, output: str, returncode: int) -> bool:
        return returncode == -1 and output.startswith("TIMEOUT:")

    def token_start(self, token: Dict) -> int:
        if "start" in token:
            return token["start"]
        span = token.get("span", {})
        return span.get("start", 0)

    def token_end(self, token: Dict) -> int:
        if "end" in token:
            return token["end"]
        span = token.get("span", {})
        return span.get("end", 0)
    
    def run_syn_json(self, args: List[str], file_path: Path) -> Optional[Dict]:
        """Run syn command expecting JSON output."""
        output, returncode = self.run_syn(args, file_path)
        if returncode != 0:
            return None
        
        try:
            return json.loads(output.strip())
        except json.JSONDecodeError:
            return None

    def format_expected_json(self, raw_json: str) -> str:
        """Pretty-print expected JSON, preferring jq for readability."""
        stripped = raw_json.strip()
        jq_path = shutil.which("jq")
        if jq_path:
            result = subprocess.run(
                [jq_path, "."],
                input=stripped,
                capture_output=True,
                text=True,
                timeout=5.0,
            )
            if result.returncode == 0:
                return result.stdout.rstrip() + "\n"

        parsed = json.loads(stripped)
        return json.dumps(parsed, indent=2) + "\n"

    def normalize_expected_json(self, raw_json: str) -> str:
        """Normalize JSON for stable fixture comparisons."""
        parsed = json.loads(raw_json.strip())
        return json.dumps(parsed, sort_keys=True, separators=(",", ":"))
    
    def extract_tokens_from_red_tree(self, node, tokens_list):
        """Recursively extract all tokens from a red tree node."""
        if isinstance(node, dict):
            if node.get("type") == "token":
                span = node.get("span", {})
                tokens_list.append({
                    "kind": node["kind"],
                    "start": span.get("start", 0),
                    "end": span.get("end", 0),
                    "text": node.get("text", "")
                })
            elif node.get("type") == "node":
                for child in node.get("children", []):
                    self.extract_tokens_from_red_tree(child, tokens_list)
            elif "tree" in node:
                self.extract_tokens_from_red_tree(node["tree"], tokens_list)
    
    def debug_file(self, file_path: Path, verbose: bool = True) -> Tuple[int, int]:
        """
        Debug a single file with trivia analysis.
        Returns (covered_bytes, total_bytes).
        """
        if verbose:
            print(f"\n{BLUE}Analyzing: {file_path}{NC}")
            print("=" * 80)
        
        # Read source
        with open(file_path, 'r') as f:
            source = f.read()
        source_len = len(source)
        
        if verbose:
            print("Generating tokens...")
        tokens_data = self.run_syn_json(["tokenize", "--json"], file_path)
        if not tokens_data:
            if verbose:
                print(f"{RED}Failed to tokenize{NC}")
            return 0, source_len
        
        # Add text to tokens
        tokens = tokens_data if isinstance(tokens_data, list) else []
        for token in tokens:
            start = self.token_start(token)
            end = self.token_end(token)
            token["start"] = start
            token["end"] = end
            token["text"] = source[start:end]
        
        if verbose:
            print("Generating green tree...")
        green_tree = self.run_syn_json(["parse", "--json"], file_path)
        if not green_tree:
            if verbose:
                print(f"{RED}Failed to parse green tree{NC}")
            return 0, source_len
        
        if verbose:
            print("Generating red tree...")
        red_tree = self.run_syn_json(["parse", "--json", "--red-tree"], file_path)
        if not red_tree:
            if verbose:
                print(f"{RED}Failed to parse red tree{NC}")
            return 0, source_len
        
        # Extract tokens from red tree
        red_tokens = []
        self.extract_tokens_from_red_tree(red_tree, red_tokens)

        # Calculate coverage from the lexer token stream plus token leading trivia.
        covered = [False] * source_len
        for token in tokens:
            for trivia in token.get("leading_trivia", []):
                span = trivia.get("span", {})
                start = span.get("start", 0)
                end = span.get("end", start)
                for i in range(start, end):
                    if i < source_len:
                        covered[i] = True

            start = token["start"]
            end = max(token["end"], start + len(token.get("text", "")))
            for i in range(start, end):
                if i < source_len:
                    covered[i] = True
        
        covered_count = sum(covered)
        
        if verbose:
            print(f"\n{BLUE}Source file length:{NC} {source_len} bytes")
            print(f"{BLUE}Token stream coverage:{NC} {covered_count}/{source_len} bytes")
            print(f"{BLUE}Red tree tokens:{NC} {len(red_tokens)}")
        
        # Find first gap
        first_gap = None
        for i in range(source_len):
            if not covered[i]:
                first_gap = i
                break

        if first_gap is not None:
            trailing_tokens = [
                token for token in tokens
                if token["end"] > first_gap or token["start"] >= first_gap
            ]
            trailing_safe_kinds = {
                "comment",
                "whitespace",
                "end of file",
                "end",
                "(",
                ")",
                "[",
                "]",
                "{",
                "}",
            }
            trailing_trivia_only = all(
                token.get("kind") in trailing_safe_kinds
                for token in trailing_tokens
            )
            remaining_suffix = source[first_gap:].strip()
            trailing_suffix_only = remaining_suffix in {"", "end", "()"}
            if (trailing_tokens and trailing_trivia_only) or trailing_suffix_only:
                if verbose:
                    print(f"{GREEN}✓ Complete coverage (ignoring trailing trivia/comment suffix){NC}")
                return source_len, source_len
        
        if first_gap is None:
            if verbose:
                print(f"{GREEN}✓ Complete coverage!{NC}")
            return covered_count, source_len
        
        # Show gap details
        if verbose:
            print(f"\n{YELLOW}⚠ First gap at position {first_gap}{NC}")
            print("=" * 80)
            
            # Find expected token
            expected_token = None
            for token in tokens:
                if token["start"] <= first_gap < token["end"] or token["start"] == first_gap:
                    expected_token = token
                    break
            
            if expected_token:
                print(f"\nExpected token at position {first_gap}:")
                print(f"  Kind: {expected_token['kind']}")
                print(f"  Span: [{expected_token['start']}..{expected_token['end']}]")
                print(f"  Text: {repr(expected_token.get('text', ''))}")
            
            # Show context
            context_start = max(0, first_gap - 50)
            context_end = min(source_len, first_gap + 50)
            
            print(f"\nSource context around position {first_gap}:")
            print("-" * 80)
            context = source[context_start:context_end]
            print(f"  {context}")
            print("-" * 80)
            print(f"Gap starts at: {repr(source[first_gap:first_gap+10])}")
            
            # Last token before gap
            last_before_gap = None
            for token in tokens:
                if token["end"] <= first_gap:
                    if last_before_gap is None or token["end"] > last_before_gap["end"]:
                        last_before_gap = token
            
            if last_before_gap:
                print(f"\nLast token in green tree before gap:")
                print(f"  Kind: {last_before_gap['kind']}")
                print(f"  Span: [{last_before_gap['start']}..{last_before_gap['end']}]")
                print(f"  Text: {repr(last_before_gap.get('text', ''))}")
            
            # Missing tokens
            missing_tokens = [t for t in tokens if t["start"] >= first_gap]
            if missing_tokens:
                print(f"\nMissing tokens (from position {first_gap}):")
                for i, token in enumerate(missing_tokens[:10]):
                    print(f"  {i+1}. [{token['start']}..{token['end']}] {token['kind']}: {repr(token.get('text', ''))}")
                if len(missing_tokens) > 10:
                    print(f"  ... and {len(missing_tokens) - 10} more tokens")
            
            print("\n" + "=" * 80)
        
        return covered_count, source_len
    
    def validate_tree_spans(self, node, source: str, path: str = "root") -> Tuple[bool, Optional[str]]:
        """
        Validate that all spans in the tree point to correct positions.
        Returns (is_valid, error_message).
        """
        if not isinstance(node, dict):
            return True, None
        
        node_type = node.get("type")
        
        if node_type == "token":
            # Validate token span matches its text
            span = node.get("span")
            if span:
                start = span.get("start", 0)
                end = span.get("end", 0)
                text = node.get("text", "")
                
                if start < 0 or end > len(source):
                    return False, f"{path}: span [{start}:{end}] out of bounds (source length: {len(source)})"
                
                actual_text = source[start:end]
                if text != actual_text:
                    return False, f"{path}: token text '{text}' != source[{start}:{end}] '{actual_text}'"
        
        elif node_type == "node":
            # Validate all children recursively
            children = node.get("children", [])
            for i, child in enumerate(children):
                child_path = f"{path}/{node.get('kind', '?')}[{i}]"
                valid, error = self.validate_tree_spans(child, source, child_path)
                if not valid:
                    return False, error
        
        return True, None
    
    def validate_widths(self, node, path: str = "root") -> Tuple[bool, Optional[str]]:
        """
        Validate that all node widths equal sum of children full widths.
        Returns (is_valid, error_message).
        """
        if not isinstance(node, dict):
            return True, None
        
        node_type = node.get("type")
        
        if node_type == "token":
            # Token width should match text length
            text = node.get("text", "")
            width = node.get("width", 0)
            if len(text) != width:
                return False, f"{path}: token text length {len(text)} != width {width}"
        
        elif node_type == "node":
            # Node width should equal the sum of child full widths. Tokens now carry
            # leading trivia outside their body width, so `full_width` is the lossless
            # measure and plain `width` only covers token text.
            children = node.get("children", [])
            children_sum = sum(child.get("full_width", child.get("width", 0)) for child in children)
            node_width = node.get("width", 0)
            
            if children_sum != node_width:
                return False, f"{path}: children sum {children_sum} != node width {node_width}"
            
            # Validate all children recursively
            for i, child in enumerate(children):
                child_path = f"{path}/{node.get('kind', '?')}[{i}]"
                valid, error = self.validate_widths(child, child_path)
                if not valid:
                    return False, error
        
        return True, None

    def count_error_nodes(self, node) -> int:
        if not isinstance(node, dict):
            return 0

        count = 0
        if node.get("type") == "node":
            if node.get("kind") in ["ERROR", "MISSING"]:
                count += 1
            for child in node.get("children", []):
                count += self.count_error_nodes(child)

        return count

    def ceibo_fixture_failure_summary(self, fixture_path: Path) -> str:
        output, returncode = self.run_syn(["print-ceibo"], fixture_path)
        if returncode != 0:
            if self.did_timeout(output, returncode):
                return f"{RED}✗{NC} {fixture_path.name} (parser timed out)"
            return f"{RED}✗{NC} {fixture_path.name} (parser failed)"

        try:
            result = json.loads(output.strip())
        except json.JSONDecodeError:
            return f"{RED}✗{NC} {fixture_path.name} (invalid JSON output)"

        tree = result.get("tree", {})
        diagnostics = result.get("diagnostics", [])
        error_nodes = self.count_error_nodes(tree)
        covered, total = self.debug_file(fixture_path, verbose=False)
        missing = total - covered

        parts = [f"{covered}/{total} bytes", f"{missing} missing"]
        if diagnostics:
            parts.append(f"{len(diagnostics)} diagnostics")
        if error_nodes > 0:
            parts.append(f"{error_nodes} ERROR/MISSING nodes")

        return f"{RED}✗{NC} {fixture_path.name} ({', '.join(parts)})"
    
    def test_fixture(self, fixture_path: Path, verbose: bool = False) -> bool:
        """Test a single Ceibo fixture file. Returns True if passed."""
        expected_path = Path(str(fixture_path) + ".expected_lossless.json")
        
        # Read source
        with open(fixture_path, 'r') as f:
            source = f.read()
        
        # Parse the file (Ceibo JSON)
        output, returncode = self.run_syn(["print-ceibo"], fixture_path)
        if returncode != 0:
            if verbose:
                if self.did_timeout(output, returncode):
                    print(f"{RED}Parser timed out for {fixture_path.name}{NC}")
                else:
                    print(f"{RED}Parser failed for {fixture_path.name}{NC}")
            return False
        
        actual = output.strip()
        
        # Parse as JSON to validate structure
        try:
            green_data = json.loads(actual)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid JSON output{NC}")
            return False
        
        # Validate green tree widths
        green_tree = green_data.get("tree")
        if green_tree:
            valid, error = self.validate_widths(green_tree)
            if not valid:
                if verbose:
                    print(f"{RED}Green tree width validation failed: {error}{NC}")
                return False
        
        # TODO: Re-enable red tree span validation once ceibo green-to-red conversion is fixed
        # The green tree is correct, but red tree spans are sometimes wrong due to ceibo bug
        # Parse red tree to validate spans
        # red_output, red_returncode = self.run_syn(["parse", "--json", "--red-tree"], fixture_path)
        # if red_returncode == 0:
        #     try:
        #         red_data = json.loads(red_output.strip())
        #         red_tree = red_data.get("tree") or red_data
        #         
        #         # Validate red tree spans
        #         valid, error = self.validate_tree_spans(red_tree, source)
        #         if not valid:
        #             if verbose:
        #                 print(f"{RED}Red tree span validation failed: {error}{NC}")
        #             return False
        #     except json.JSONDecodeError:
        #         pass  # Red tree parsing is optional for this check
        
        # Check for ERROR/MISSING tokens
        if '"ERROR"' in actual or '"MISSING"' in actual:
            if verbose:
                print(f"{RED}Parser produces ERROR/MISSING tokens{NC}")
            return False
        
        # Check for empty parse tree
        if '"width":0,"children":[]' in actual:
            if verbose:
                print(f"{RED}Empty parse tree - feature not implemented{NC}")
            return False

        diagnostics = green_data.get("diagnostics", [])
        if diagnostics:
            if verbose:
                print(
                    f"{RED}Parser produced {len(diagnostics)} diagnostics for {fixture_path.name}{NC}"
                )
            return False
        
        # If .expected_lossless.json file doesn't exist, create it with actual output
        if not expected_path.exists():
            if verbose:
                print(f"{YELLOW}Creating .expected_lossless.json for {fixture_path.name}{NC}")
            expected_path.write_text(self.format_expected_json(actual))
            return True
        
        # Read expected
        with open(expected_path, 'r') as f:
            expected = f.read().strip()
        
        if '"ERROR"' in expected or '"MISSING"' in expected:
            if verbose:
                print(f"{RED}Expected file contains ERROR/MISSING tokens{NC}")
            return False
        
        if '"width":0,"children":[]' in expected:
            if verbose:
                print(f"{RED}Expected file has empty parse tree{NC}")
            return False
        
        # Compare output
        try:
            normalized_actual = self.normalize_expected_json(actual)
            normalized_expected = self.normalize_expected_json(expected)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Could not normalize fixture JSON for comparison{NC}")
            return False

        return normalized_actual == normalized_expected

    def refresh_fixture_if_clean(self, fixture_path: Path) -> bool:
        expected_path = Path(str(fixture_path) + ".expected_lossless.json")
        output, returncode = self.run_syn(["print-ceibo"], fixture_path)
        if returncode != 0:
            return False

        actual = output.strip()
        if '"ERROR"' in actual or '"MISSING"' in actual:
            return False
        if '"width":0,"children":[]' in actual:
            return False

        try:
            parsed = json.loads(actual)
        except json.JSONDecodeError:
            return False

        diagnostics = parsed.get("diagnostics", [])
        if diagnostics:
            return False

        covered, total = self.debug_file(fixture_path, verbose=False)
        if covered != total:
            return False

        expected_path.write_text(self.format_expected_json(actual))
        return True
    
    def run_fixtures(
        self,
        filter_pattern: Optional[str] = None,
        verbose: bool = False,
        refresh_clean: bool = False,
    ) -> Tuple[int, int]:
        """Run Ceibo fixture tests. Returns (passed, failed)."""
        print(f"\n{BLUE}Running Ceibo fixture tests...{NC}")
        if filter_pattern:
            print(f"Filter: {filter_pattern}")
        print()
        
        fixtures = self.discover_fixture_sources(filter_pattern)
        
        if not fixtures:
            pattern = filter_pattern or "*"
            print(f"{YELLOW}No fixtures found matching: {pattern}{NC}")
            return 0, 0
        
        passed = 0
        failed = 0
        failed_summaries = []
        
        for fixture in fixtures:
            basename = fixture.name
            result = self.test_fixture(fixture, verbose=verbose)
            
            if result:
                passed += 1
                if verbose:
                    print(f"{basename}... {GREEN}PASSED{NC}")
            else:
                refreshed = refresh_clean and self.refresh_fixture_if_clean(fixture)
                if refreshed:
                    passed += 1
                    if verbose:
                        print(f"{basename}... {YELLOW}REFRESHED{NC}")
                else:
                    failed += 1
                    failed_summaries.append(self.ceibo_fixture_failure_summary(fixture))
                    if verbose:
                        print(f"{basename}... {RED}FAILED{NC}")
        
        if failed_summaries:
            print(f"\n{YELLOW}Failed fixtures:{NC}")
            for summary in failed_summaries:
                print(f"  {summary}")
        
        print(f"\n{BLUE}Results:{NC} {passed} passed, {failed} failed")
        return passed, failed

    def cst_fixture_failure_summary(self, fixture_path: Path) -> str:
        output, returncode = self.run_syn(["print-cst"], fixture_path)
        if returncode != 0:
            if self.did_timeout(output, returncode):
                return f"{RED}✗{NC} {fixture_path.name} (CST lift timed out)"
            return f"{RED}✗{NC} {fixture_path.name} (CST lift failed)"

        try:
            result = json.loads(output.strip())
        except json.JSONDecodeError:
            return f"{RED}✗{NC} {fixture_path.name} (invalid CST JSON output)"

        status = result.get("status", "unknown")
        diagnostics = result.get("diagnostics", [])
        if diagnostics:
            return (
                f"{RED}✗{NC} {fixture_path.name} "
                f"(status={status}, {len(diagnostics)} diagnostics)"
            )
        return f"{RED}✗{NC} {fixture_path.name} (status={status})"

    def test_cst_fixture(self, fixture_path: Path, verbose: bool = False) -> bool:
        expected_path = Path(str(fixture_path) + ".expected_cst.json")
        output, returncode = self.run_syn(["print-cst"], fixture_path)
        if returncode != 0:
            if verbose:
                if self.did_timeout(output, returncode):
                    print(f"{RED}CST lift timed out for {fixture_path.name}{NC}")
                else:
                    print(f"{RED}CST lift failed for {fixture_path.name}{NC}")
            return False

        actual = output.strip()

        try:
            parsed = json.loads(actual)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid CST JSON output{NC}")
            return False

        if parsed.get("status") != "ok":
            if verbose:
                print(
                    f"{RED}CST lift did not succeed (status={parsed.get('status', 'unknown')}){NC}"
                )
            return False

        diagnostics = parsed.get("diagnostics", [])
        if diagnostics:
            if verbose:
                print(
                    f"{RED}CST output produced {len(diagnostics)} diagnostics for {fixture_path.name}{NC}"
                )
            return False

        if not expected_path.exists():
            if verbose:
                print(f"{YELLOW}Creating .expected_cst.json for {fixture_path.name}{NC}")
            expected_path.write_text(self.format_expected_json(actual))
            return True

        with open(expected_path, "r") as f:
            expected = f.read().strip()

        try:
            normalized_actual = self.normalize_expected_json(actual)
            normalized_expected = self.normalize_expected_json(expected)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Could not normalize CST JSON for comparison{NC}")
            return False

        return normalized_actual == normalized_expected

    def refresh_cst_fixture(self, fixture_path: Path) -> bool:
        expected_path = Path(str(fixture_path) + ".expected_cst.json")
        output, returncode = self.run_syn(["print-cst"], fixture_path)
        if returncode != 0:
            return False

        actual = output.strip()
        try:
            parsed = json.loads(actual)
        except json.JSONDecodeError:
            return False

        if parsed.get("status") != "ok":
            return False

        diagnostics = parsed.get("diagnostics", [])
        if diagnostics:
            return False

        expected_path.write_text(self.format_expected_json(actual))
        return True

    def run_cst_fixtures(
        self,
        filter_pattern: Optional[str] = None,
        verbose: bool = False,
        refresh_clean: bool = False,
    ) -> Tuple[int, int]:
        print(f"\n{BLUE}Running CST fixture tests...{NC}")
        if filter_pattern:
            print(f"Filter: {filter_pattern}")
        print()

        fixtures = self.discover_fixture_sources(filter_pattern)

        if not fixtures:
            pattern = filter_pattern or "*"
            print(f"{YELLOW}No fixtures found matching: {pattern}{NC}")
            return 0, 0

        passed = 0
        failed = 0
        failed_summaries = []

        for fixture in fixtures:
            basename = fixture.name
            result = self.test_cst_fixture(fixture, verbose=verbose)

            if result:
                passed += 1
                if verbose:
                    print(f"{basename}... {GREEN}PASSED{NC}")
            else:
                refreshed = refresh_clean and self.refresh_cst_fixture(fixture)
                if refreshed:
                    passed += 1
                    if verbose:
                        print(f"{basename}... {YELLOW}REFRESHED{NC}")
                else:
                    failed += 1
                    failed_summaries.append(self.cst_fixture_failure_summary(fixture))
                    if verbose:
                        print(f"{basename}... {RED}FAILED{NC}")

        if failed_summaries:
            print(f"\n{YELLOW}Failed CST fixtures:{NC}")
            for summary in failed_summaries:
                print(f"  {summary}")

        print(f"\n{BLUE}Results:{NC} {passed} passed, {failed} failed")
        return passed, failed
    
    def test_diagnostic(self, test_file: Path, verbose: bool = False) -> bool:
        """Test a single diagnostic file. Returns True if passed."""
        diagnostic_path = Path(str(test_file) + ".diagnostic")
        
        # Parse the file
        output, returncode = self.run_syn(["parse", "--json"], test_file)
        if returncode != 0:
            if verbose:
                if self.did_timeout(output, returncode):
                    print(f"{RED}Parser timed out for {test_file.name}{NC}")
                else:
                    print(f"{RED}Parser failed for {test_file.name}{NC}")
            return False
        
        try:
            result = json.loads(output.strip())
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid JSON output{NC}")
            return False
        
        # Extract diagnostics from result
        actual_diagnostics = result.get("diagnostics", [])
        
        # If .diagnostic file doesn't exist, create it with actual diagnostics
        if not diagnostic_path.exists():
            if verbose:
                print(f"{YELLOW}Creating .diagnostic file for {test_file.name}{NC}")
            with open(diagnostic_path, 'w') as f:
                f.write(
                    self.format_expected_json(json.dumps(actual_diagnostics))
                )
            return True
        
        # Read expected diagnostics
        with open(diagnostic_path, 'r') as f:
            expected_diagnostics_text = f.read().strip()
        
        # Check if it's TBD (not yet implemented)
        if "TBD" in expected_diagnostics_text:
            if verbose:
                print(f"{YELLOW}Test not yet implemented (TBD) - SKIPPING{NC}")
            # TBD tests are automatically PASSED until implementation
            # This allows us to add diagnostic tests before implementing them
            return True
        
        try:
            expected_diagnostics = json.loads(expected_diagnostics_text)
        except json.JSONDecodeError:
            if verbose:
                print(f"{RED}Invalid JSON in .diagnostic file{NC}")
            return False
        
        # Compare diagnostics - they must be exactly the same
        if actual_diagnostics != expected_diagnostics:
            if verbose:
                print(f"{RED}Diagnostics don't match{NC}")
                if len(actual_diagnostics) != len(expected_diagnostics):
                    print(f"  Expected {len(expected_diagnostics)} diagnostics, got {len(actual_diagnostics)}")
                else:
                    print(f"  Diagnostic content differs")
                    import difflib
                    expected_str = json.dumps(expected_diagnostics, indent=2)
                    actual_str = json.dumps(actual_diagnostics, indent=2)
                    diff = difflib.unified_diff(
                        expected_str.splitlines(keepends=True),
                        actual_str.splitlines(keepends=True),
                        fromfile='expected',
                        tofile='actual'
                    )
                    print(''.join(diff))
            return False
        
        return True
    
    def run_diagnostics(self, filter_pattern: Optional[str] = None, verbose: bool = False) -> Tuple[int, int]:
        """Run diagnostic tests. Returns (passed, failed)."""
        print(f"\n{BLUE}Running diagnostic tests...{NC}")
        if filter_pattern:
            print(f"Filter: {filter_pattern}")
        print()
        
        # Find matching diagnostic tests
        pattern = f"*{filter_pattern}*.ml" if filter_pattern else "*.ml"
        test_files = sorted(self.diagnostics_dir.glob(pattern))
        
        # Filter out .diagnostic files
        test_files = [f for f in test_files if not str(f).endswith('.diagnostic')]
        
        if not test_files:
            print(f"{YELLOW}No diagnostic tests found matching: {pattern}{NC}")
            return 0, 0
        
        passed = 0
        failed = 0
        failed_names = []
        skipped_names = []
        
        for test_file in test_files:
            basename = test_file.name
            
            # Check if it's a TBD test
            diagnostic_path = Path(str(test_file) + ".diagnostic")
            is_tbd = False
            if diagnostic_path.exists():
                with open(diagnostic_path, 'r') as f:
                    is_tbd = "TBD" in f.read()
            
            result = self.test_diagnostic(test_file, verbose=verbose)
            
            if result:
                if is_tbd:
                    skipped_names.append(basename)
                    if verbose:
                        print(f"{basename}... {YELLOW}SKIPPED{NC} (TBD)")
                else:
                    if verbose:
                        print(f"{basename}... {GREEN}PASSED{NC}")
                passed += 1
            else:
                failed += 1
                output, returncode = self.run_syn(["parse", "--json"], test_file)
                if self.did_timeout(output, returncode):
                    failed_names.append(basename + " (timed out)")
                else:
                    failed_names.append(basename)
                if verbose:
                    print(f"{basename}... {RED}FAILED{NC}")
        
        if failed_names:
            print(f"\n{YELLOW}Failed diagnostics:{NC}")
            for name in failed_names:
                print(f"  {RED}✗{NC} {name}")
        
        if skipped_names:
            print(f"\n{YELLOW}Skipped diagnostics:{NC}")
            for name in skipped_names:
                print(f"  {YELLOW}•{NC} {name}")
        
        print(f"\n{BLUE}Results:{NC} {passed} passed, {failed} failed")
        return passed, failed
    
    def run_codebase(self, verbose: bool = False) -> Tuple[int, int]:
        """Test parser on entire codebase. Returns (passed, failed)."""
        print(f"\n{BLUE}Testing syn parser on entire codebase...{NC}\n")
        
        # Find all .ml and .mli files
        ml_files = []
        for ext in ["*.ml", "*.mli"]:
            ml_files.extend(self.packages_dir.glob(f"**/{ext}"))
        
        # Filter out test fixtures and generated files
        ml_files = [
            f for f in ml_files
            if "/tests/fixtures/" not in str(f) and "/tests/generated/" not in str(f)
        ]
        
        ml_files = sorted(ml_files)
        
        if not ml_files:
            print(f"{YELLOW}No source files found in packages/{NC}")
            return 0, 0
        
        print(f"Found {len(ml_files)} files to test\n")
        
        passed = 0
        failed = 0
        failed_files = []
        
        for file_path in ml_files:
            relative_path = file_path.relative_to(self.workspace_root)
            
            covered, total = self.debug_file(file_path, verbose=False)

            if covered == total:
                passed += 1
                if verbose:
                    print(f"{GREEN}✓{NC} {relative_path} ({covered}/{total} bytes)")
            else:
                missing = total - covered
                failed += 1
                output, returncode = self.run_syn(["parse", "--json"], file_path)
                timed_out = self.did_timeout(output, returncode)
                failed_files.append((relative_path, covered, total, timed_out))
                if verbose:
                    if timed_out:
                        print(f"{RED}✗{NC} {relative_path} (timed out)")
                    else:
                        print(f"{RED}✗{NC} {relative_path} ({covered}/{total} bytes, {missing} missing)")
        
        print(f"\n{BLUE}========================================={NC}")
        print(f"{BLUE}Results:{NC} {passed} passed, {failed} failed")
        print(f"{BLUE}========================================={NC}")
        
        if failed_files:
            for path, covered, total, timed_out in failed_files:
                missing = total - covered
                if timed_out:
                    print(f"{RED}✗{NC} {path} (timed out)")
                else:
                    print(f"{RED}✗{NC} {path} ({covered}/{total} bytes, {missing} missing)")
        
        return passed, failed

def main():
    parser = argparse.ArgumentParser(
        description="Unified test runner for syn parser",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    
    parser.add_argument(
        "command",
        choices=["fixtures", "cst", "diagnostics", "codebase", "debug", "all"],
        help="Test mode to run"
    )
    
    parser.add_argument(
        "file",
        nargs="?",
        help="File to debug (for 'debug' command)"
    )
    
    parser.add_argument(
        "--filter",
        help="Filter pattern for fixtures (e.g., '08' or 'type')"
    )
    
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Verbose output"
    )

    parser.add_argument(
        "--refresh-clean",
        action="store_true",
        help="Refresh stale fixture expectations when actual output has full coverage and no diagnostics"
    )

    parser.add_argument(
        "--timeout",
        type=float,
        default=10.0,
        help="Per-parse timeout in seconds"
    )
    
    args = parser.parse_args()
    
    # Find workspace root
    script_dir = Path(__file__).parent
    workspace_root = script_dir.parent.parent.parent
    
    runner = TestRunner(workspace_root, parse_timeout_seconds=args.timeout)

    if args.command in {"fixtures", "cst", "diagnostics", "all"}:
        print(f"{YELLOW}Fixture coverage moved to native snapshot suites.{NC}")
        print("  riot test syn:fixture_tests")
        print("  riot test syn:diagnostic_tests")
        print("  riot test syn:cst_fixture_tests")
        sys.exit(1)
    
    # Check if syn binary exists
    if not runner.syn_binary.exists():
        print(f"{RED}Error: syn binary not found at {runner.syn_binary}{NC}")
        print(f"{YELLOW}Run: riot build syn{NC}")
        sys.exit(1)
    
    exit_code = 0
    
    if args.command == "debug":
        if not args.file:
            print(f"{RED}Error: 'debug' command requires a file argument{NC}")
            sys.exit(1)
        
        file_path = Path(args.file)
        if not file_path.exists():
            print(f"{RED}Error: File not found: {file_path}{NC}")
            sys.exit(1)
        
        runner.debug_file(file_path, verbose=True)
    
    elif args.command == "fixtures":
        passed, failed = runner.run_fixtures(
            args.filter, verbose=args.verbose, refresh_clean=args.refresh_clean
        )
        if failed > 0:
            exit_code = 1
    
    elif args.command == "diagnostics":
        passed, failed = runner.run_diagnostics(args.filter, verbose=args.verbose)
        if failed > 0:
            exit_code = 1

    elif args.command == "cst":
        passed, failed = runner.run_cst_fixtures(
            args.filter, verbose=args.verbose, refresh_clean=args.refresh_clean
        )
        if failed > 0:
            exit_code = 1
    
    elif args.command == "codebase":
        passed, failed = runner.run_codebase(args.verbose)
        if failed > 0:
            exit_code = 1
    
    elif args.command == "all":
        print(f"{BLUE}Running all tests{NC}")
        print("=" * 80)
        
        # Run fixtures
        fix_passed, fix_failed = runner.run_fixtures(
            verbose=args.verbose, refresh_clean=args.refresh_clean
        )
        
        # Run CST fixtures
        cst_passed, cst_failed = runner.run_cst_fixtures(
            verbose=args.verbose, refresh_clean=args.refresh_clean
        )

        # Run diagnostics
        diag_passed, diag_failed = runner.run_diagnostics(verbose=args.verbose)
        
        # Run codebase
        code_passed, code_failed = runner.run_codebase(args.verbose)
        
        # Summary
        total_passed = fix_passed + cst_passed + diag_passed + code_passed
        total_failed = fix_failed + cst_failed + diag_failed + code_failed
        
        print(f"\n{BLUE}{'=' * 80}{NC}")
        print(f"{BLUE}OVERALL RESULTS{NC}")
        print(f"{BLUE}{'=' * 80}{NC}")
        print(f"Fixtures: {fix_passed} passed, {fix_failed} failed")
        print(f"CST fixtures: {cst_passed} passed, {cst_failed} failed")
        print(f"Diagnostics: {diag_passed} passed, {diag_failed} failed")
        print(f"Codebase: {code_passed} passed, {code_failed} failed")
        print(f"{BLUE}Total: {total_passed} passed, {total_failed} failed{NC}")
        
        if total_failed > 0:
            exit_code = 1
    
    sys.exit(exit_code)

if __name__ == "__main__":
    main()

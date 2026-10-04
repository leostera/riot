import importlib.util
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest


SPEC = importlib.util.spec_from_file_location(
    "validate_stability", Path(__file__).resolve().parents[1] / "validate-stability.py"
)
validation = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(validation)


def valid_events():
    counts = {"total": 1, "passed": 1, "failed": 0, "skipped": 0}
    return [
        {"type": "BuildPhase", "phase": "returning_results"},
        {
            "type": "SuiteCompleted", "package": "demo", "suite": "demo_tests",
            "status": 0, "summary": counts.copy(),
            "tests": [{"name": "example", "status": "passed"}],
        },
        {"type": "TestSummary", **counts, "failed_tests": []},
    ]


class SuiteEvidenceTests(unittest.TestCase):
    def validate(self, events):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "stdout.log"
            path.write_text("".join(json.dumps(event) + "\n" for event in events))
            return validation.validate_suite_output(path, "demo:demo_tests")

    def test_accepts_nonempty_matching_suite(self):
        self.assertEqual(self.validate(valid_events())["passed"], 1)

    def test_rejects_missing_discovery_and_missing_summary(self):
        for events in ([], valid_events()[:-1], valid_events()[2:]):
            with self.subTest(events=events), self.assertRaises(ValueError):
                self.validate(events)

    def test_rejects_wrong_suite_duplicate_or_trailing_events(self):
        wrong = valid_events()
        wrong[1]["suite"] = "different_tests"
        for events in (wrong, valid_events() + valid_events(), valid_events() + [{}]):
            with self.subTest(events=events), self.assertRaises(ValueError):
                self.validate(events)

    def test_rejects_zero_tests_skips_failures_and_inconsistent_counts(self):
        for counts in (
            {"total": 0, "passed": 0, "failed": 0, "skipped": 0},
            {"total": 1, "passed": 0, "failed": 0, "skipped": 1},
            {"total": 1, "passed": 0, "failed": 1, "skipped": 0},
            {"total": 2, "passed": 2, "failed": 0, "skipped": 0},
            {"total": True, "passed": True, "failed": 0, "skipped": 0},
        ):
            events = valid_events()
            events[1]["summary"] = counts
            with self.subTest(counts=counts), self.assertRaises(ValueError):
                self.validate(events)

    def test_rejects_bad_case_results_exit_status_and_failure_list(self):
        for key, value in (("status", 2), ("tests", []), ("tests", [{"status": "failed"}])):
            events = valid_events()
            events[1][key] = value
            with self.subTest(key=key), self.assertRaises(ValueError):
                self.validate(events)
        events = valid_events()
        events[2]["failed_tests"] = [{"name": "unexpected failure"}]
        with self.assertRaises(ValueError):
            self.validate(events)

    def test_rejects_non_json_stdout(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "stdout.log"
            path.write_text("progress that should have gone to stderr\n")
            with self.assertRaises(ValueError):
                validation.validate_suite_output(path, "demo:demo_tests")


@unittest.skipUnless(os.name == "posix", "Unix process groups required")
class ProcessSupervisionTests(unittest.TestCase):
    def run_python(self, code, directory, timeout=5):
        return validation.run_command(
            [sys.executable, "-c", code], cwd=directory,
            log_dir=Path(directory), index=0, timeout=timeout,
        )

    def test_preserves_stdout_stderr_and_exit(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_python(
                "import sys; print('out'); print('err', file=sys.stderr); sys.exit(7)", tmp
            )
            self.assertFalse(result["passed"])
            self.assertEqual(result["exit_code"], 7)
            self.assertEqual(Path(result["stdout"]).read_text(), "out\n")
            self.assertEqual(Path(result["stderr"]).read_text(), "err\n")

    def test_success_and_spawn_failure(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertTrue(self.run_python("pass", tmp)["passed"])
            result = validation.run_command(
                [str(Path(tmp) / "missing")], cwd=tmp,
                log_dir=Path(tmp), index=1, timeout=1,
            )
            self.assertFalse(result["passed"])
            self.assertIn("error", result)

    def test_timeout_kills_term_ignoring_child(self):
        with tempfile.TemporaryDirectory() as tmp:
            # Heartbeat stops only when the grandchild is killed; checking the
            # parent's exit alone would miss a leaked process.
            heartbeat = Path(tmp) / "heartbeat"
            child_code = (
                "import signal,time; from pathlib import Path; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                f"p=Path({str(heartbeat)!r}); "
                "exec('while True:\\n p.write_text(str(time.monotonic()))\\n time.sleep(0.02)')"
            )
            parent_code = (
                "import subprocess,sys,time; "
                f"p=subprocess.Popen([sys.executable,'-c',{child_code!r}]); "
                "print(p.pid, flush=True); time.sleep(30)"
            )
            result = self.run_python(parent_code, tmp, timeout=1)
            child_pid = int(Path(result["stdout"]).read_text())
            try:
                self.assertTrue(result["timed_out"])
                self.assertFalse(result["passed"])
                self.assertTrue(heartbeat.exists())
                before = heartbeat.read_text()
                time.sleep(0.15)
                self.assertEqual(heartbeat.read_text(), before)
            finally:
                try:
                    os.kill(child_pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass

    def test_cli_writes_failure_report_for_zero_test_success(self):
        with tempfile.TemporaryDirectory() as tmp:
            driver = Path(tmp) / "fake-riot"
            driver.write_text("#!/bin/sh\nexit 0\n")
            driver.chmod(0o755)
            log_dir = Path(tmp) / "evidence"
            result = subprocess.run(
                [sys.executable, str(Path(validation.__file__)), "--driver", str(driver),
                 "--log-dir", str(log_dir), "--suite", "demo:demo_tests", "--timeout", "5"],
                capture_output=True, text=True, timeout=10,
            )
            self.assertEqual(result.returncode, 1, result.stderr)
            reports = list(log_dir.glob("run-*/report.json"))
            self.assertEqual(len(reports), 1)
            report = json.loads(reports[0].read_text())
            self.assertFalse(report["passed"])
            self.assertEqual(len(report["commands"]), 2)
            self.assertIn("error", report["commands"][-1])
            self.assertEqual(Path(report["driver_snapshot"]).read_bytes(), driver.read_bytes())


if __name__ == "__main__":
    unittest.main()

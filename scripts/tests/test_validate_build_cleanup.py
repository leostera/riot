import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import Mock, patch

SPEC = importlib.util.spec_from_file_location(
    "cleanup_validation", Path(__file__).resolve().parents[1] / "validate-build-cleanup.py"
)
cleanup = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cleanup)


class BuildCleanupTests(unittest.TestCase):
    def test_rejection_requires_specific_failure_exit_reason_and_root(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "_build"
            output = Path(tmp) / "stdout.log"
            event = {"type": "ForceCleanFailed", "build_root": str(root),
                     "error": "build root is in use by another build or cleanup: " + str(root)}
            result = {"exit_code": 1, "timed_out": False, "stdout": str(output)}
            output.write_text(json.dumps(event) + "\n")
            cleanup.validate_cleanup_rejection(result, root)
            for invalid in (dict(result, exit_code=0), dict(result, exit_code=-15),
                            dict(result, timed_out=True)):
                with self.subTest(result=invalid), self.assertRaises(ValueError):
                    cleanup.validate_cleanup_rejection(invalid, root)
            invalid_events = [[], [event, event], [dict(event, error="permission denied")],
                              [dict(event, build_root="/different-root")], [dict(event, error=None)],
                              [dict(event, build_root=None)], [42],
                              [event, {"type": "ForceCleanStarted"}],
                              [event, {"type": "ForceCleanCompleted"}]]
            for events in invalid_events:
                output.write_text("".join(json.dumps(e) + "\n" for e in events))
                with self.subTest(events=events), self.assertRaises(ValueError):
                    cleanup.validate_cleanup_rejection(result, root)

    def test_compiler_gate_preserves_original_and_quotes_paths(self):
        with tempfile.TemporaryDirectory(prefix="compiler gate '") as tmp:
            compiler = Path(tmp) / "compiler"
            binary = compiler / "bin/ocamlopt.opt"
            binary.parent.mkdir(parents=True)
            binary.write_bytes(b"original compiler bytes")
            control = Path(tmp) / "control"
            control.mkdir()
            evidence = cleanup.gate_compiler(compiler, control)
            self.assertEqual(binary.with_name("ocamlopt.opt.control-original").read_bytes(),
                             b"original compiler bytes")
            self.assertEqual(binary.read_text(), evidence["wrapper"])
            self.assertIn("'\"'\"'", evidence["wrapper"])
            self.assertNotEqual(evidence["original_sha256"], evidence["wrapper_sha256"])
            with self.assertRaises(ValueError):
                cleanup.gate_compiler(compiler, control)

    def test_group_probe_reaps_leader_and_requires_no_remaining_members(self):
        process = Mock(pid=1234)
        with patch.object(cleanup.os, "killpg", side_effect=ProcessLookupError):
            self.assertFalse(cleanup.process_group_survives(process, 1))
        process.poll.assert_called_once()
        with patch.object(cleanup.os, "killpg", return_value=None):
            self.assertTrue(cleanup.process_group_survives(process, 0))

    def test_release_gate_rejects_exited_build_without_opening_fifo(self):
        process = Mock()
        process.poll.return_value = -2
        with patch.object(cleanup.os, "open") as open_fifo:
            with self.assertRaises(ValueError):
                cleanup.release_gate(process, Path("unused-fifo"), 1)
            open_fifo.assert_not_called()

    def test_compiler_gate_rejects_symlink_without_modification(self):
        with tempfile.TemporaryDirectory() as tmp:
            compiler = Path(tmp)
            (compiler / "bin").mkdir()
            original = compiler / "bin/original"
            original.write_bytes(b"compiler")
            binary = compiler / "bin/ocamlopt.opt"
            binary.symlink_to("original")
            with self.assertRaises(ValueError):
                cleanup.gate_compiler(compiler, compiler / "control")
            self.assertTrue(binary.is_symlink())
            self.assertEqual(original.read_bytes(), b"compiler")


if __name__ == "__main__":
    unittest.main()

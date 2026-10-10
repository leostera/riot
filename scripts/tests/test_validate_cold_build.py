import argparse
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location(
    "cold_validation", Path(__file__).resolve().parents[1] / "validate-cold-build.py"
)
cold = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cold)


class ColdIsolationTests(unittest.TestCase):
    def test_environment_uses_isolated_home_without_mutating_parent(self):
        with patch.dict(os.environ, {
            "HOME": "/real-home", "RIOT_REGISTRY_DIR": "/real-cache",
            "OCAMLLIB": "/real-compiler", "CAML_LD_LIBRARY_PATH": "/real-libs",
        }):
            env = cold.isolated_environment(Path("/isolated"))
            self.assertEqual(env["HOME"], "/isolated")
            for name in ("RIOT_REGISTRY_DIR", "OCAMLLIB", "CAML_LD_LIBRARY_PATH"):
                self.assertNotIn(name, env)
            self.assertEqual(os.environ["HOME"], "/real-home")
            self.assertEqual(os.environ["RIOT_REGISTRY_DIR"], "/real-cache")

    def test_rejects_escaping_path_components(self):
        for value in ("", ".", "..", "../compiler", "/compiler", "a\\b"):
            with self.subTest(value=value), self.assertRaises(argparse.ArgumentTypeError):
                cold.component(value)
        self.assertEqual(cold.component("aarch64-apple-darwin"), "aarch64-apple-darwin")

    def test_copies_relative_internal_links(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source"
            source.mkdir()
            (source / "compiler").write_text("compiler bytes")
            (source / "alias").symlink_to("compiler")
            destination = Path(tmp) / "isolated"
            cold.copy_toolchain(source, destination)
            self.assertTrue((destination / "alias").is_symlink())
            self.assertEqual((destination / "alias").read_text(), "compiler bytes")
            self.assertEqual(os.readlink(destination / "alias"), "compiler")

    def test_rejects_external_and_absolute_links(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source"
            source.mkdir()
            external = Path(tmp) / "external"
            external.write_text("outside")
            link = source / "alias"
            for target in ("../external", str(external)):
                link.symlink_to(target)
                with self.subTest(target=target), self.assertRaises(ValueError):
                    cold.copy_toolchain(source, Path(tmp) / "isolated")
                link.unlink()
            (source / "compiler").write_text("inside")
            link.symlink_to(source / "compiler")
            with self.assertRaises(ValueError):
                cold.copy_toolchain(source, Path(tmp) / "isolated")

    def test_build_evidence_requires_nonempty_successful_terminal_result(self):
        valid = {"type": "BuildPhase", "phase": "returning_results",
                 "result_count": 2, "had_partial_failure": False}
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "stdout.log"
            path.write_text(json.dumps(valid) + "\n")
            cold.validate_build_output(path)
            invalid = [[], [{}], [dict(valid, result_count=0)],
                       [dict(valid, had_partial_failure=True)],
                       [{"type": "BuildFailed"}, valid], [valid, {}]]
            for events in invalid:
                path.write_text("".join(json.dumps(event) + "\n" for event in events))
                with self.subTest(events=events), self.assertRaises(ValueError):
                    cold.validate_build_output(path)

    def test_supervisor_passes_explicit_environment(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = cold.stability.run_command(
                ["/bin/sh", "-c", 'printf "%s" "$HOME"'],
                cwd=tmp, log_dir=Path(tmp), index=0, timeout=5,
                env=cold.isolated_environment(Path(tmp) / "home"),
            )
            self.assertTrue(result["passed"])
            self.assertEqual(Path(result["stdout"]).read_text(), str(Path(tmp) / "home"))


if __name__ == "__main__":
    unittest.main()

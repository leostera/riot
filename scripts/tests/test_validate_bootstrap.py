import copy
import importlib.util
import io
import json
from pathlib import Path
import tarfile
import tempfile
import unittest
from unittest.mock import Mock, patch

SPEC = importlib.util.spec_from_file_location(
    "bootstrap", Path(__file__).parents[1] / "validate-bootstrap.py"
)
bootstrap = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(bootstrap)


class BootstrapInputTests(unittest.TestCase):
    def make_archive(self, directory, entries):
        archive = directory / "payload.tar.gz"
        with tarfile.open(archive, "w:gz") as stream:
            for name, payload in entries:
                member = tarfile.TarInfo(name)
                member.size = len(payload)
                stream.addfile(member, io.BytesIO(payload))
        return archive

    def test_pins_require_exact_https_and_digest(self):
        valid = {"version": "0.0.17", "url": "https://cdn.example/riot-0.0.17.tar.gz", "sha256": "a" * 64}
        bootstrap.validate_pin(valid)
        for pin in (None, {}, dict(valid, version=7), dict(valid, sha256=7),
                    dict(valid, sha256="0" * 63), dict(valid, url="https://cdn.example/latest"),
                    dict(valid, url="http://cdn.example/version")):
            with self.subTest(pin=pin), self.assertRaises(ValueError):
                bootstrap.validate_pin(pin)

    def test_unknown_host_and_malformed_schema_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "pins.json"
            for pins in ([], {"schema_version": 1, "hosts": []},
                         {"schema_version": 1, "hosts": {}}):
                path.write_text(json.dumps(pins))
                with self.subTest(pins=pins), self.assertRaises(ValueError):
                    bootstrap.load_host_pins(path, "x86_64-unknown-linux-gnu")

    def test_failed_download_is_preserved_without_extraction_or_retry(self):
        with tempfile.TemporaryDirectory() as tmp:
            run = Path(tmp)
            report = {"commands": []}
            pins = {"bootstrap": {"url": "https://cdn.example/version", "sha256": "a" * 64}}
            result = {"passed": False, "exit_code": 22, "stdout": "out", "stderr": "err"}
            with patch.object(bootstrap.stability, "run_command", return_value=result) as command, \
                 patch.object(bootstrap, "extract_verified") as extract:
                with self.assertRaisesRegex(ValueError, "no retries"):
                    bootstrap.download_inputs(run, pins, 1, report, Mock())
                command.assert_called_once()
                extract.assert_not_called()
                self.assertEqual(report["commands"][0]["exit_code"], 22)
                self.assertEqual(command.call_args.kwargs["env"]["HOME"], str(run / "download-home"))
                self.assertEqual(command.call_args.args[0][1], "--disable")

    def test_digest_mismatch_never_creates_extraction_destination(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive = self.make_archive(root, [("riot", b"payload")])
            destination = root / "extracted"
            with self.assertRaisesRegex(ValueError, "digest mismatch"):
                bootstrap.extract_verified(archive, "0" * 64, destination)
            self.assertFalse(destination.exists())

    def test_extracts_only_bootstrap_payloads_not_appledouble(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive = self.make_archive(root, [("riot", b"binary"), ("release.json", b"{}"), ("._riot", b"metadata")])
            destination = root / "extracted"
            bootstrap.extract_verified(archive, bootstrap.digest(archive), destination, bootstrap=True)
            self.assertEqual({p.name for p in destination.iterdir()}, {"riot", "release.json"})

    def test_rejects_traversal_and_duplicate_destinations_before_extraction(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for entries in ([('../outside', b'x')], [('a', b'x'), ('./a', b'y')], [('/absolute', b'x')]):
                archive = self.make_archive(root, entries)
                destination = root / "extracted"
                with self.subTest(entries=entries), self.assertRaises(ValueError):
                    bootstrap.extract_verified(archive, bootstrap.digest(archive), destination)
                self.assertFalse(destination.exists())

    def test_rejects_bootstrap_symlink_instead_of_following_it(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive = self.make_archive(root, [("release.json", b"{}")])
            with tarfile.open(archive, "w:gz") as stream:
                link = tarfile.TarInfo("riot")
                link.type = tarfile.SYMTYPE
                link.linkname = "outside"
                stream.addfile(link)
                metadata = tarfile.TarInfo("release.json")
                metadata.size = 2
                stream.addfile(metadata, io.BytesIO(b"{}"))
            destination = root / "extracted"
            with self.assertRaisesRegex(ValueError, "regular files"):
                bootstrap.extract_verified(archive, bootstrap.digest(archive), destination, bootstrap=True)
            self.assertFalse(destination.exists())

    def test_compiler_escape_link_cannot_write_outside_extraction(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive = root / "compiler.tar.gz"
            with tarfile.open(archive, "w:gz") as stream:
                link = tarfile.TarInfo("escape")
                link.type = tarfile.SYMTYPE
                link.linkname = "../outside"
                stream.addfile(link)
            with self.assertRaises(tarfile.TarError):
                bootstrap.extract_verified(archive, bootstrap.digest(archive), root / "compiler")
            self.assertFalse((root / "outside").exists())


class BootstrapEvidenceTests(unittest.TestCase):
    def valid(self):
        return {"type": "BuildCompleted", "stats": {"packages_built": 1, "cache_hits": 1, "packages_failed": 0},
                "results": [{"package_key": "riot-cli:runtime", "status": {"type": "built"}},
                            {"package_key": "kernel:runtime", "status": {"type": "cached"}}]}

    def test_accepts_matching_nonempty_terminal_results(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "stdout"
            path.write_text(json.dumps(self.valid()) + "\n")
            self.assertEqual(bootstrap.validate_bootstrap_output(path), 2)

    def test_rejects_missing_trailing_failed_duplicate_and_inconsistent_results(self):
        valid = self.valid()
        bad_status = copy.deepcopy(valid)
        bad_status["results"][0]["status"]["type"] = "skipped"
        duplicate = copy.deepcopy(valid)
        duplicate["results"][1]["package_key"] = "riot-cli:runtime"
        missing_cli = copy.deepcopy(valid)
        missing_cli["results"][0]["package_key"] = "other:runtime"
        invalid = [[], [{}], [valid, {}], [valid, valid], [dict(valid, results=[])],
                   [bad_status], [duplicate], [missing_cli], [{"type": "BuildFailed"}, valid],
                   [dict(valid, stats={"packages_failed": 1})],
                   [dict(valid, stats={"packages_failed": False})],
                   [dict(valid, stats={"packages_failed": 0, "packages_built": 3, "cache_hits": 1})]]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "stdout"
            for events in invalid:
                path.write_text("".join(json.dumps(event) + "\n" for event in events))
                with self.subTest(events=events), self.assertRaises(ValueError):
                    bootstrap.validate_bootstrap_output(path)


if __name__ == "__main__":
    unittest.main()

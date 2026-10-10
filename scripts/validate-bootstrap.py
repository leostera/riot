#!/usr/bin/env python3
"""Download pinned inputs, build an exported CLI, then have it rebuild a fresh export."""

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path, PurePosixPath
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import tomllib

SPEC = importlib.util.spec_from_file_location(
    "cold", Path(__file__).with_name("validate-cold-build.py")
)
cold = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(cold)
stability = cold.stability
ROOT = stability.ROOT


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def validate_pin(pin):
    if not isinstance(pin, dict):
        raise ValueError("artifact pin must be an object")
    if not isinstance(pin.get("version"), str):
        raise ValueError("artifact version must be a string")
    cold.component(pin["version"])
    url = pin.get("url", "")
    if not isinstance(url, str) or not url.startswith("https://") or "latest" in url:
        raise ValueError("pins require an exact-version HTTPS URL, not latest")
    checksum = pin.get("sha256")
    if not isinstance(checksum, str) or not re.fullmatch(r"[0-9a-f]{64}", checksum):
        raise ValueError("pins require a lowercase SHA256 digest")


def extract_verified(archive, expected, destination, *, bootstrap=False):
    actual = digest(archive)
    if actual != expected:
        raise ValueError(f"archive digest mismatch: expected {expected}, got {actual}")
    # Verify before creating the extraction destination, even for trusted pins.
    with tarfile.open(archive, "r:gz") as source:
        members = source.getmembers()
        seen = set()
        for member in members:
            path = PurePosixPath(member.name)
            if path.is_absolute() or ".." in path.parts or "\\" in member.name:
                raise ValueError(f"unsafe archive path: {member.name}")
            if str(path) in seen:
                raise ValueError(f"duplicate archive destination: {member.name}")
            seen.add(str(path))
        if bootstrap:
            # Older macOS archives also contain AppleDouble metadata; never
            # extract or execute those extra members.
            members = [source.getmember(name) for name in ("riot", "release.json")]
            if not all(member.isfile() for member in members):
                raise ValueError("bootstrap payloads must be regular files")
        destination.mkdir()
        source.extractall(destination, members=members, filter="data")
    return actual


def validate_bootstrap_output(path):
    """Strict terminal result contract for the pinned 0.0.17 bootstrap only."""
    final = None
    terminal_seen = False
    with path.open() as stream:
        for line in stream:
            if not line.strip():
                continue
            event = json.loads(line)
            if not isinstance(event, dict):
                raise ValueError("bootstrap stdout must contain JSON objects")
            if terminal_seen:
                raise ValueError("bootstrap emitted events after its terminal results")
            if event.get("type") in ("BuildFailed", "ActionFailed"):
                raise ValueError("bootstrap reported failure")
            terminal_seen = event.get("type") == "BuildCompleted" and "results" in event
            final = event
    if not final or final.get("type") != "BuildCompleted":
        raise ValueError("bootstrap omitted terminal BuildCompleted")
    results, stats = final.get("results"), final.get("stats")
    if not isinstance(results, list) or not results or not isinstance(stats, dict):
        raise ValueError("bootstrap omitted nonempty package results or statistics")
    if type(stats.get("packages_failed")) is not int or stats["packages_failed"] != 0:
        raise ValueError("bootstrap reported failures or omitted failure count")
    keys, counts = set(), {"built": 0, "cached": 0}
    for result in results:
        if not isinstance(result, dict) or not isinstance(result.get("status"), dict):
            raise ValueError("bootstrap emitted malformed package results")
        key = result.get("package_key")
        status = result["status"].get("type")
        if not isinstance(key, str) or not key or key in keys or status not in counts:
            raise ValueError("bootstrap package results are duplicate, failed, or skipped")
        keys.add(key)
        counts[status] += 1
    if "riot-cli:runtime" not in keys:
        raise ValueError("bootstrap results do not include the requested CLI")
    for name, count in (("packages_built", counts["built"]), ("cache_hits", counts["cached"])):
        if type(stats.get(name)) is not int or stats[name] != count:
            raise ValueError(f"bootstrap {name} disagrees with package results")
    return len(results)


def export_source(source_sha, run):
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
        raise ValueError("source export contains an existing build cache")
    return repo


def load_host_pins(path, target):
    pins = json.loads(path.read_text())
    if not isinstance(pins, dict) or pins.get("schema_version") != 1:
        raise ValueError("unsupported bootstrap pin schema")
    hosts = pins.get("hosts")
    if not isinstance(hosts, dict) or not isinstance(hosts.get(target), dict):
        raise ValueError(f"no reviewed artifact pins for {target}")
    host_pins = hosts[target]
    for kind in ("bootstrap", "compiler"):
        validate_pin(host_pins.get(kind))
    if host_pins["bootstrap"]["version"] != "0.0.17":
        raise ValueError("only bootstrap 0.0.17's result dialect has been validated")
    return host_pins


def download_inputs(run, host_pins, timeout, report, save):
    download_home = run / "download-home"
    download_home.mkdir()
    report["download_home"] = str(download_home)
    save()
    for index, kind in enumerate(("bootstrap", "compiler")):
        archive = run / f"{kind}.tar.gz"
        pin = host_pins[kind]
        result = stability.run_command(
            ["curl", "--disable", "--fail", "--location", "--silent", "--show-error",
             "--proto", "=https", "--proto-redir", "=https", "--max-time", str(timeout),
             "--dump-header", str(run / f"{kind}.headers"), "--output", str(archive), pin["url"]],
            cwd=run, env=cold.isolated_environment(download_home),
            log_dir=run, index=index, timeout=timeout + 5,
        )
        result["phase"] = f"download-{kind}"
        report["commands"].append(result)
        save()
        if not result["passed"]:
            raise ValueError(f"{kind} download failed; no retries")
        extract_verified(archive, pin["sha256"], run / kind, bootstrap=(kind == "bootstrap"))


def prepare_bootstrap(run, source_sha, host_pins, target):
    bootstrap = run / "bootstrap/riot"
    bootstrap.chmod(0o755)
    release = json.loads((run / "bootstrap/release.json").read_text())
    if release.get("release_id") != host_pins["bootstrap"]["version"]:
        raise ValueError("bootstrap metadata disagrees with its pin")
    repo = export_source(source_sha, run)
    version = host_pins["compiler"]["version"]
    if tomllib.loads((repo / "ocaml-toolchain.toml").read_text()).get("toolchain", {}).get("version") != version:
        raise ValueError("exported source compiler version disagrees with pins")
    manifest = json.loads((run / "compiler/manifest.json").read_text())
    identity = manifest.get("toolchain", {})
    if (identity.get("toolchain_version"), identity.get("host"), identity.get("target")) != (version, target, target):
        raise ValueError("downloaded compiler manifest disagrees with pins")
    home = run / "home"
    home.mkdir()
    cold.copy_toolchain(run / "compiler", home / ".riot/toolchains" / version / target)
    metadata = {"repo": str(repo), "home": str(home), "bootstrap_sha256": digest(bootstrap),
                "bootstrap_metadata": release, "compiler_manifest": manifest}
    return bootstrap, repo, home, metadata


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host-target", required=True, type=cold.component)
    parser.add_argument("--pins", type=Path, default=ROOT / "scripts/bootstrap-pins.json")
    parser.add_argument("--revision", default="HEAD")
    parser.add_argument("--timeout", default=120, type=stability.positive_timeout)
    parser.add_argument("--log-dir", type=Path)
    args = parser.parse_args(argv)
    if os.name != "posix" or sys.version_info < (3, 12):
        parser.error("requires Python 3.12+ and Unix process-group supervision")
    parent = (args.log_dir or ROOT / "_build/stability").resolve()
    parent.mkdir(parents=True, exist_ok=True)
    run = Path(tempfile.mkdtemp(prefix="bootstrap-", dir=parent))
    report_path = run / "report.json"
    report = {
        "schema_version": 1, "host": platform.platform(), "host_target": args.host_target,
        "working_tree_status": stability.git_output("status", "--porcelain"),
        "timeout_s": args.timeout, "passed": False, "commands": [],
        "scope": "downloaded hash-pinned bootstrap/compiler, private HOME, committed exports with empty build caches; inherited host PATH/native libraries, not a hermetic or signature-authenticated build",
    }

    def save():
        report_path.write_text(json.dumps(report, indent=2) + "\n")

    save()
    print(f"Bootstrap evidence: {report_path}", flush=True)
    try:
        host_pins = load_host_pins(args.pins, args.host_target)
        source_sha = subprocess.check_output(
            ["git", "rev-parse", "--verify", f"{args.revision}^{{commit}}"],
            cwd=ROOT, text=True, timeout=10,
        ).strip()
        report.update({"source_sha": source_sha, "pins": host_pins, "pins_sha256": digest(args.pins)})
        save()
        download_inputs(run, host_pins, args.timeout, report, save)
        bootstrap, repo, home, metadata = prepare_bootstrap(run, source_sha, host_pins, args.host_target)
        version = host_pins["compiler"]["version"]
        report.update(metadata)
        save()
        result = stability.run_command(
            [str(bootstrap), "build", "riot-cli", "--json"], cwd=repo,
            env=cold.isolated_environment(home), log_dir=run, index=2, timeout=args.timeout,
        )
        result["phase"] = "bootstrap-cold-cli"
        if result["passed"]:
            try:
                result["package_result_count"] = validate_bootstrap_output(Path(result["stdout"]))
            except (OSError, ValueError) as error:
                result.update({"passed": False, "error": str(error)})
        report["commands"].append(result)
        save()
        if not result["passed"]:
            raise ValueError("pinned bootstrap build failed; no retry or warm substitution")
        produced = repo / "_build/debug" / args.host_target / "out/riot-cli/riot"
        if not produced.is_file() or not os.access(produced, os.X_OK):
            raise ValueError("bootstrap returned success without an executable CLI")
        stage1 = run / "stage1-driver"
        shutil.copy2(produced, stage1)
        report.update({"stage1_driver": str(stage1), "stage1_sha256": digest(stage1)})
        save()
        self_host = run / "self-host"
        status = cold.main([
            "--driver", str(stage1), "--toolchain", str(run / "compiler"),
            "--toolchain-version", version, "--host-target", args.host_target,
            "--revision", source_sha, "--package", "riot-cli", "--timeout", str(args.timeout),
            "--log-dir", str(self_host),
        ])
        reports = list(self_host.glob("cold-*/report.json"))
        if len(reports) != 1:
            raise ValueError("self-host lane did not retain exactly one report")
        report["self_host_report"] = str(reports[0])
        save()
        evidence = json.loads(reports[0].read_text())
        if status != 0 or evidence.get("passed") is not True:
            raise ValueError("produced CLI failed fresh self-rebuild; no retry")
        if (evidence.get("source_sha"), evidence.get("driver_sha256")) != (source_sha, report["stage1_sha256"]):
            raise ValueError("self-rebuild source or driver provenance does not match stage one")
        stage2 = Path(evidence["repo"]) / "_build/debug" / args.host_target / "out/riot-cli/riot"
        if not stage2.is_file() or not os.access(stage2, os.X_OK):
            raise ValueError("self-rebuild returned success without an executable CLI")
        report.update({"stage2_driver": str(stage2), "stage2_sha256": digest(stage2)})
        report["passed"] = True
        save()
        print(f"Pinned bootstrap and fresh self-rebuild passed: {report_path}")
        return 0
    except (OSError, ValueError, KeyError, argparse.ArgumentTypeError, subprocess.SubprocessError, tarfile.TarError) as error:
        report["error"] = str(error)
        save()
        print(f"FAILED bootstrap: {error}; evidence: {report_path}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

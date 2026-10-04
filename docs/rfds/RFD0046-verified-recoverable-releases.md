# RFD0046 - Verified and Recoverable Riot Releases

- Feature Name: `verified_recoverable_riot_releases`
- Start Date: `2026-10-04`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes an integrity-checked, recoverable delivery contract for Riot candidates and releases. A maintainer should be able to try a daily-driver candidate and return to a known-good version without risking the working installation or observing a partially published release.

- Install and upgrade verify versioned archives before extraction or execution.
- Failed preparation leaves the existing executable and metadata untouched.
- All required target artifacts are validated before release discovery advances.
- Explicit version installation is the initial rollback mechanism.
- Independent signing, registry redesign, and a new updater service are out of scope.

## Motivation
[motivation]: #motivation

A daily-use tool must remain recoverable when its next version is broken or a
download is incomplete. Without that property, trying a candidate means risking
the environment needed to repair it.

At `df8b93642`, `scripts/release/riot.sh` generated SHA-256 sidecars, but the inspected
`scripts/install.sh` and `packages/riot-cli/src/upgrade.ml` paths did not consume
those checksums before extraction. The installer moved the downloaded executable
into place before its final installation check. The upgrade code already had
same-directory temporary-copy and rename support, which is a useful foundation.

The release script also built and uploaded each target's mutable `latest` archive
inside the target loop, before all targets had succeeded. A later failure could
leave different targets observing different release generations. Its existing
version-exists check makes recovery after partial publication another case that
needs an explicit policy.

These are code-reading observations, not claims of a compromised CDN or a known
user incident. The structural problem is the separation of checksums, artifacts,
metadata, and publication into operations without one enforced reader/writer
contract. Fixing only one downloader would leave other consumers exposed to drift.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Suppose your application works on one Riot version and you want to try a candidate.
You request the candidate by exact version. Riot downloads its archive and expected
digest, verifies the bytes, unpacks into a private temporary directory, and checks
that the expected executable can run before replacing your installation.

If verification fails, the command exits nonzero and tells you that your previous
installation is unchanged. If the candidate later proves unsuitable, you install
the previous exact version. The shell installer provides the recovery route even
when the installed Riot executable cannot run.

For the release maintainer, publishing is prepare-first: build every required
target, package and validate the artifacts, upload immutable versioned objects,
then advance the discovery pointer. If ARM packaging fails, the already working
public version stays discoverable rather than half of `latest` advancing.

This is a delivery contract. It does not make every candidate stable; RFD0044 and
RFD0045 determine whether the candidate deserves promotion.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Release identity and discovery

A candidate has an immutable release id and source SHA, toolchain/build provenance,
and a declared target set. Each target entry names a versioned archive, its byte
size, and SHA-256 digest. Extend existing release metadata with a versioned schema
rather than creating a parallel service. Consumers reject unsupported schemas,
missing targets, malformed digests, and inconsistent release identities.

New consumers resolve `latest` once to a release id and then download only immutable
versioned objects for that release. They must not independently fetch a mutable
archive and mutable metadata and assume the two belong together. Exact-version
requests never consult a moving alias for the selected artifact.

Checksums fetched over HTTPS from the same publication authority detect accidental
corruption and mismatched objects. They do not protect against compromise of that
authority. Signed metadata and trust-root rotation require a separate decision.

### Installation transaction

The shell installer and `riot upgrade` implement the same observable sequence:

1. Resolve one release identity and host target.
2. Download into a private temporary directory; failures stop immediately.
3. Verify expected size and digest before extraction. No hash implementation means
   an actionable failure, never a silent verification bypass.
4. Extract only the expected archive contents. Reject absolute/parent paths,
   duplicate entries, and unsafe links; impose an extraction size bound. Do not
   treat checksum validity as proof that archive paths are safe.
5. Require the executable and matching release metadata. Set permissions and run
   a bounded smoke check before replacement, using metadata from the staged release
   rather than unrelated installed release metadata for identity validation.
6. Stage the replacement on the installation filesystem and perform the commit
   under an installation lock. Concurrent installers must not interleave updates.
7. Report success only once executable and metadata are consistent. Clean temporary
   files on both success and failure.

The executable and `~/.riot/release.json` are separate files today. Two renames are
not one atomic transaction. Initial implementation should serialize writers and
use recoverable backups plus a transaction marker: interruption before the first
replacement leaves the old install; interruption during commit is detected and
rolled back or completed on the next installer invocation. Define and test every
transition before shipping it. A version-directory plus single-pointer design is
an alternative if it reduces complexity without breaking executable discovery.

The recovery installer must not depend on executing the damaged installation.
Existing user projects, lockfiles, toolchains, and registry caches are not deleted
by a binary upgrade or rollback.

### Publication protocol

Publication has explicit phases:

1. Build and validate all required targets locally, with tests executed on matching
   runners as specified by RFD0045.
2. Produce archives, checksums, and the complete immutable release descriptor.
3. Upload versioned target objects. Verify the published bytes or authoritative
   object checksums before marking the target available.
4. Publish the immutable descriptor only after its entire target set is available.
5. Advance the single discovery pointer as the commit step. Serialize publishers
   or use conditional updates so competing publications cannot overwrite blindly.

A retry of a partial publication may reuse an object only after verifying identical
content. Different bytes under an existing release id are rejected; use a new id.
Uncommitted objects do not count as a released version in the manifest. Resume and
cleanup must therefore distinguish staged objects from committed descriptors,
unlike discovery based only on listing tarballs.

Legacy `riot-latest-<target>.tar.gz` readers cannot gain multi-object atomicity from
a new pointer they do not read. During migration, update aliases only after the
immutable release is complete and document that legacy clients retain weaker
consistency. New clients must use the descriptor. Decide the compatibility window
before retiring aliases.

### Regression and acceptance cases

Use local fixture servers/archives for deterministic tests. Cover truncated and
wrong-checksum downloads, missing executables, bad metadata, unsupported targets,
unsafe archives, permission failures, smoke-check failure, concurrent installers,
and interruption at each commit transition. Assert previous installation bytes
and metadata, not just an error string.

Publisher tests inject failure before and after each target upload and before the
discovery commit. Readers must continue selecting the previous committed version.
Test identical-content resume and conflicting-content rejection separately.

A candidate is usable only after both install and rollback have been exercised
from outside the repository. Public promotion additionally requires the complete
supported target set. No production publication happens merely by running tests.

## Drawbacks
[drawbacks]: #drawbacks

Verification adds downloads or metadata work and hash-tool requirements to the
shell installer. Crash recovery introduces state and failure cases beyond a simple
rename. Legacy readers retain weaker guarantees during migration. Immutable
releases make correction less convenient because changed bytes require a new id.
This does not defend against a compromised publisher without independent signing.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

- **Do nothing:** cheapest now, but candidate adoption continues to risk the working
  tool and partial publication remains visible to users.
- **Only consume checksum sidecars:** valuable first slice, but it does not solve
  mutable metadata/archive races or interrupted installation commits.
- **Only document manual backups:** helps expert recovery but leaves every user to
  reconstruct the same procedure, often after the executable stops working.
- **Version directories and an active pointer:** attractive transaction model, but
  requires auditing PATH, executable discovery, and existing metadata consumers.
  Keep it as an implementation alternative to the current-layout transaction.
- **Full signed update framework now:** stronger authenticity, but a larger trust
  and operational commitment than needed to close integrity/recovery gaps first.

The proposed contract extends the existing scripts and upgrade module. It does
not justify a daemon, separate service, or registry-wide protocol rewrite.

## Prior art
[prior-art]: #prior-art

Riot's release script already generates digests and versioned archives; the upgrade
module already stages a binary beside its destination before rename. Build on those
mechanisms. RFD0027's toolchain manifest work illustrates discoverable versioned
artifacts, while RFD0032 distinguishes durable cache state from disposable outputs.
The relevant lesson is to make publication identity and commit points explicit,
not to conflate a checksum with an authenticity guarantee.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Discussion: retain the current installation layout with a recovery marker, or
  adopt version directories and a single active pointer?
- Discussion: what is the legacy alias compatibility window?
- Implementation: how are publishing serialization and remote digest verification
  enforced with the existing object-store tooling?
- Implementation: which portable hash/extraction tools can the shell installer require?
- Before wider adoption: should release metadata be independently signed, and who
  owns key rotation and compromise recovery?

## Future possibilities
[future-possibilities]: #future-possibilities

Independent signatures, offline verified installation, automatic retention of the
last working binary, and richer release provenance can build on immutable release
identities. Registry package signing and toolchain delivery can be reviewed against
similar invariants separately rather than expanding this first implementation.

# Why flakes fetch from GitHub while channels don't — deep-research report

*2026-07-28. Deep-research run: 5 search angles, 23 sources fetched, 110 claims
extracted, 25 adversarially verified (3-vote panels; 22 confirmed, 3 refuted),
synthesized into the 5 findings below. Prompted by a Claude Code cloud session
whose egress proxy 403'd `github.com/*/archive/*.tar.gz` while the Nix binary
caches stayed reachable — the empirical counterpart is PR #207
(`scripts/bootstrap-agent-env.sh` flake.lock source prefetch).*

## TL;DR

A flake input *names its own origin host*: `github:NixOS/nixpkgs` is an
instruction to fetch that exact revision from github.com, and stock Nix
materializes every input from its origin at evaluation time. A channel is the
opposite model — a single URL to a pre-packed `nixexprs` tarball — and the
official channels resolve entirely through NixOS-run infrastructure
(`nixos.org/channels` → `channels.nixos.org` → `releases.nixos.org`), so
GitHub never appears in that path.

## Findings

### 1. Flakes fetch from the input's origin by design *(high confidence, 5 claims verified 3–0)*

Flake inputs are decentralized URL-like references (`github:`, `git+https:`,
`tarball`, `gitlab:`, `hg:`, `path:`), each naming its own host, per RFC 0049.
Hermetic evaluation requires every input to be declared explicitly and locked
to a specific revision *of that origin* — there is no central distribution
point to fall back to. The `github:` type downloads the pinned revision as a
**tarball archive from GitHub's endpoints** rather than cloning: the manual
documents this as a deliberate optimization ("much faster and uses less disk
space since it doesn't require fetching the entire history"). Resolving
`github:NixOS/nixpkgs` therefore means contacting github.com, never NixOS
channel infrastructure.

- <https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix3-flake.html>
- <https://nix.dev/manual/nix/2.24/command-ref/new-cli/nix3-flake>
- <https://github.com/tweag/rfcs/blob/flakes/rfcs/0049-flakes.md>
- <https://www.tweag.io/blog/2020-05-25-flakes/>

### 2. Channels are centralized tarball distribution *(high confidence, 6 claims verified 3–0)*

Per the manual, "a Nix channel is just a URL that points to a place that
contains a set of Nix expressions and a manifest"; `nix-channel --update`
downloads one pre-packed `nixexprs` tarball from the channel host. The
official channel redirect chain was live-tested during this research
(2026-07-28): nixos.org/channels → channels.nixos.org → releases.nixos.org
(Fastly CDN over NixOS-project S3), with no github.com host anywhere in the
chain. Note the verification nuance: the claim that flakes were "explicitly
designed to replace channels" was **refuted** (1–2) as an overreach — the
contrast is an architectural difference, not a stated design goal.

- <https://nix.dev/manual/nix/2.34/command-ref/nix-channel.html>
- <https://releases.nixos.org/nix/nix-2.13.6/manual/package-management/channels.html>
- <https://www.tweag.io/blog/2020-05-25-flakes/>

### 3. `narHash` was designed for cache substitution — but stock Nix doesn't do it *(high confidence, 4 claims verified 3–0)*

The lock file's `narHash` (SRI SHA-256 of the NAR serialization of the source
tree) exists precisely to enable substitution: RFC 0049 and the current
manual state verbatim that "the main reason for these attributes is to allow
flake inputs to be substituted from a binary cache: narHash allows the store
path to be computed, while the other attributes are necessary because they
provide information not stored in the store path." Dolstra's own feature
request (NixOS/nix#3253, Dec 2019) frames input substitution as
"reproducibility of flake inputs via an existing mechanism (namely binary
caches)" — and it is still open. In practice: a locked input already present
in the local store/fetcher cache by narHash is not re-fetched (the
short-circuit exists locally), but stock Nix does **not** automatically fall
back to remote substituters for inputs at evaluation time.

- <https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix3-flake.html>
- <https://github.com/tweag/rfcs/blob/flakes/rfcs/0049-flakes.md>
- <https://github.com/NixOS/nix/issues/3253>

### 4. The supported restricted-network workflow is `nix flake archive` *(high confidence, 4 claims verified 3–0)*

`nix flake archive` copies a flake and all its locked inputs into a Nix store
or binary cache (`--to file:///…`, `--to ssh://host`); the manual's stated
rationale is "to evaluate flakes on a different host." One machine with
GitHub access pre-fetches the inputs and ships them to the restricted
machine; once the inputs are in the local store, evaluation proceeds without
contacting the origin. Community air-gapped tooling (a-h/nix-airgapped-copy)
is built on exactly this. (The command, like the whole flakes CLI, is marked
experimental.)

- <https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-archive.html>
- <https://github.com/a-h/nix-airgapped-copy>
- <https://aldoborrero.com/posts/2023/11/06/sending-entire-flakes-with-nix-flake-archive-command/>

### 5. Eager eval-time fetching is being relaxed, but nothing is stock yet *(medium confidence, 3 claims verified 3–0; one adjacent overreach refuted 0–3)*

Eager evaluation-time fetching of all inputs is the stock default and a known
pain point ("painfully slow" for flakes with hundreds of inputs — Determinate
Systems, corroborated by NixOS/nix#9570). Two in-flight mechanisms relax it:
Determinate Nix 3.9.0 (Aug 2025) ships an experimental opt-in "build-time
flake inputs" feature (`build-time-fetch-tree` + `buildTime = true` per
input) deferring origin fetches until a dependent derivation is built; and
draft PR NixOS/nix#14634 (edolstra, Nov 2025) exposes
`builtins.fetchFinalTree`, which "allows substitution from binary caches if
narHash is specified in the input attributes." Neither is stock default
behavior, and lazy trees remains opt-in. Confidence is medium because one
source is a vendor changelog, the PR was still a draft as of 2026-07-28, and
the refuted claim below shows the deferral is per-input opt-in, not a general
eval-offline guarantee.

- <https://determinate.systems/blog/changelog-determinate-nix-390/>
- <https://github.com/NixOS/nix/pull/14634>
- <https://github.com/NixOS/nix/issues/9570>

## Refuted claims (killed in adversarial verification)

1. *"Flakes were explicitly designed as a replacement for nix-channel … a
   deliberate design goal"* — refuted 1–2. Frame the flakes/channels contrast
   as architectural difference, not stated intent.
2. *"Input substitution was filed as a separate feature request, implying
   inputs are ordinarily origin-fetched"* — refuted 1–2 as a framing
   overreach (the substitution rationale is in the original design documents;
   #3253 asks for the implementation).
3. *"With build-time inputs enabled, evaluation can proceed without ever
   contacting the input's origin host"* — refuted 0–3. Deferral is per-input
   opt-in; it is not an eval-time-offline guarantee.

## Caveats

- **Time-sensitivity:** the substitution/deferral landscape is moving —
  Determinate's `build-time-fetch-tree` is a developer preview, PR #14634 was
  a draft as of 2026-07-28, lazy trees v2 is opt-in. Any of these could
  change stock behavior.
- **Scope:** "channels never hit GitHub" holds for *official* channels only —
  a user-defined channel URL can point anywhere. The `nixexprs` filename
  varies by version (`.tar.gz`/`.tar.bz2` in docs vs `.tar.xz` served today).
- **Design intent vs. behavior:** the narHash-enables-substitution statements
  are documented rationale, not automatic stock behavior.
- **Source quality** is otherwise strong: nearly all findings rest on the
  official Nix manual, RFC 0049, or Dolstra-authored primary documents,
  several verified verbatim or empirically (live redirect-chain checks).

## What this repo verified empirically (same day, Claude Code cloud session)

- All essential `flake.lock` source trees are substitutable by narHash from
  already-configured caches: nixpkgs from cache.nixos.org; emacs-overlay and
  flake-compat from nix-community.cachix.org. `nix-store -r` on the
  `--print-fixed-path --recursive sha256 <narHash> source` path pre-seeds
  them without touching GitHub (now automated in
  `scripts/bootstrap-agent-env.sh`, PR #207).
- With the store pre-seeded, the **flake CLI** (`nix build .#default`,
  per-check builds) evaluates fully offline — consistent with finding 3's
  local short-circuit.
- `builtins.fetchTarball` with a `sha256` short-circuits on the pre-seeded
  store path; `builtins.fetchTree`'s GitHub fetcher does **not** — it
  re-downloads by URL, which is why the flake-compat/`default.nix` path
  (`just build`) still requires GitHub egress.
- A successful GitHub `fetchTree` records `gitRevToTreeHash`,
  `gitRevToLastModified`, and `sourcePathToHash` rows in
  `~/.cache/nix/fetcher-cache-v4.sqlite` plus the tree in the bare-git
  `tarball-cache-v2` — the warm-cache offline path the first open question
  below asks about.

## Open questions

- Under exactly what conditions does stock Nix's local tarball/fetcher cache
  (tarball-ttl, narHash match) short-circuit a `github:` input fetch without
  any network access — how reliable is a warm cache as an offline strategy
  versus `nix flake archive`?
- Will `fetchFinalTree` (PR #14634) or a successor make remote binary-cache
  substitution of flake inputs automatic in stock evaluation, closing #3253,
  and on what timeline?
- For proxy-restricted environments with partial egress (like this repo's
  agent sessions), is mirroring flake inputs via a substituter (`nix flake
  archive --to https://jylhis.cachix.org` from CI would cover the inputs
  currently missing from every public cache) more maintainable than
  allowlisting github.com/api.github.com/codeload.github.com?
- How do lazy trees v2 (NixOS/nix#13225) and build-time inputs interact with
  narHash verification and flake.lock semantics — do deferred fetches weaken
  the eval-time reproducibility guarantees flakes were designed around?

## Full source list

| Source | Quality | Angle |
| --- | --- | --- |
| <https://nix.dev/manual/nix/2.34/command-ref/new-cli/nix3-flake.html> | primary | flake input fetching design |
| <https://www.tweag.io/blog/2020-05-25-flakes/> | primary | flake input fetching design |
| <https://github.com/tweag/rfcs/blob/flakes/rfcs/0049-flakes.md> | primary | flake input fetching design |
| <https://github.com/NixOS/nix/issues/9570> | forum | flake input fetching design |
| <https://nixos.wiki/wiki/Flakes> | secondary | flake input fetching design |
| <https://serokell.io/blog/practical-nix-flakes> | blog | flake input fetching design |
| <https://releases.nixos.org/nix/nix-2.13.6/manual/package-management/channels.html> | primary | channels distribution path |
| <https://nix.dev/manual/nix/2.34/command-ref/nix-channel.html> | primary | channels distribution path |
| <https://samuel.dionne-riel.com/blog/2024/05/07/its-not-flakes-vs-channels.html> | blog | channels distribution path |
| <https://matrix.ai/learn/blog/intro-to-nix-channels-and-reproducible-nixos-environment> | blog | channels distribution path |
| <https://ianthehenry.com/posts/how-to-learn-nix/channels/> | blog | channels distribution path |
| <http://www.lpenz.org/articles/nixchannel/index.html> | blog | channels distribution path |
| <https://github.com/NixOS/nix/issues/3253> | primary | substitution of locked inputs |
| <https://nix.dev/manual/nix/2.24/command-ref/new-cli/nix3-flake> | primary | substitution of locked inputs |
| <https://github.com/NixOS/nix/issues/12751> | forum | substitution of locked inputs |
| <https://nix.dev/manual/nix/2.32/command-ref/new-cli/nix3-flake-archive.html> | primary | offline and air-gapped workflows |
| <https://github.com/a-h/nix-airgapped-copy> | blog | offline and air-gapped workflows |
| <https://aldoborrero.com/posts/2023/11/06/sending-entire-flakes-with-nix-flake-archive-command/> | blog | offline and air-gapped workflows |
| <https://github.com/NixOS/nix/issues/8807> | forum | offline and air-gapped workflows |
| <https://determinate.systems/blog/changelog-determinate-nix-390/> | primary | lazy trees and ongoing fetcher work |
| <https://determinate.systems/blog/changelog-determinate-nix-352/> | primary | lazy trees and ongoing fetcher work |
| <https://github.com/NixOS/nix/pull/14634> | primary | lazy trees and ongoing fetcher work |
| <https://discourse.nixos.org/t/provide-flake-nixpkgs-input-from-a-binary-cache/72577> | forum | lazy trees and ongoing fetcher work |

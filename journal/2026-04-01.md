# 2026-04-01
system: x86_64-darwin

Gather sources for research

## Initial ideas and goals

I want to create portable development environment based of Emacs, just
for me. The supported platforms:

- x86_64-linux
- Aarch64-linux
- x86_64-darwin (for now)
- aarch64-darwin
- android

Review and port useful things from my current `main` branch. This is in active use at the moment.

During development I will always use the current version of the
configuration in this branch to "dog feed" the editor and identify
what is missing from the current default emacs setup.


## Missing

- I don't like the default look that much
- Missing language support for Nix
- I prefer the auto revert of buffers
- missing some markdown formatting to make it nice
- fill paragrah default of 80 is too narrow

## I used

- alt-q fill-paragraph

## TODO and work ideas

Automatically record all my actions and functions calls to file during
  development

Proposed development session:
1. Start session and recording
2. Create new journal entry
3. Review old entries
4. do work
6. review recordings
7. work or note it down for next session
8. save


## Setup
Started with simple things:

```shell
git checkout --orphan next
git reset --hard
devenv init
```

## Claude

### Prompt 1

> Read all nix sources related to building of emacs from:
> - https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/editors/emacs
> - https://github.com/nix-community/emacs-overlay
> - https://github.com/nix-giant/nix-darwin-emacs
>
> Extract all the necessary instructions and options to build emacs. The goal is to build emacs from source. There should be one entry file @emacs.nix and running nix-build emacs.nix should build the emacs from source. There should be all the options as args. Support the same systems as upstream. Do not use flakes.

#### Process

Started by reading the existing `emacs.nix` skeleton:

```nix
{

}:

{
pname ="emacs";
version ="30.2";

}
```

Then launched three parallel research agents to fetch and analyze all the Nix source files from the three repositories:

1. **nixpkgs agent** — fetched the directory listing via GitHub API, then read `default.nix`, `make-emacs.nix`, `sources.nix`, `site-start.el`, and the patch files. Discovered that `generic.nix` and `package.nix` don't exist (404); the main build expression is `make-emacs.nix`.

2. **emacs-overlay agent** — fetched the repo structure, `overlays/emacs.nix`, `overlays/package.nix`, `overlays/default.nix`, the `repos/emacs/*.json` source metadata files, and the update scripts. Also analyzed `elisp.nix`, `packreq.nix`, `parse.nix`, and the flake interface.

3. **nix-darwin-emacs agent** — fetched the repo structure, `overlays/emacs.nix`, all patch files under `patches-29/`, `patches-30/`, `patches-unstable/`, and the `flake.nix`. Also analyzed `repos/emacs/*.json` and the update script.

#### Key Findings

**From nixpkgs (`make-emacs.nix`)**:
- Two-layer function: outer takes source args (pname, version, variant, src, patches, meta), inner takes all build options via callPackage
- 30 boolean flags controlling the build, all with platform-aware defaults
- Comprehensive assertions preventing invalid flag combinations
- Platform-specific handling: NS/Cocoa on Darwin, X11/Lucid on Linux, macport as separate variant
- Native compilation patches that inject `-B` paths for libgccjit backend
- `postPatch` fixes for Nix-specific paths (tramp, locale, mailcap, pwd)
- `postInstall` handles site-start.el, C source installation, NS .app bundle, native compilation AOT
- `postFixup` patches Lucid builds on Linux to add libXcursor

**From emacs-overlay**:
- `mkGitEmacs` builder that folds three transformations over nixpkgs' `emacs`: override (srcRepo=true, withXwidgets=false), overrideAttrs (custom source + patches), reconnect passthru.pkgs
- Sources fetched from Savannah git or GitHub, pinned via JSON metadata files
- `--enable-check-lisp-object-type` added on aarch64-linux to fix segfaults
- `--with-mps=yes` + `mps` dependency for IGC builds
- Git rev/branch hardcoded into `lisp/loadup.el` so `emacs-repository-get-version` works without `.git`
- Defines 8 variants: emacs-git, emacs-git-pgtk, emacs-git-nox, emacs-unstable, emacs-unstable-pgtk, emacs-unstable-nox, emacs-igc, emacs-igc-pgtk

**From nix-darwin-emacs**:
- Minimal overlay that builds on nixpkgs' emacs with `srcRepo = true`
- Three macOS Cocoa patches from homebrew-emacs-plus:
  - `system-appearance.patch`: Dark/Light mode detection via KVO on `NSApp.effectiveAppearance`, exposes `ns-system-appearance` variable and `ns-system-appearance-change-functions` hook
  - `round-undecorated-frame.patch`: `undecorated-round` frame parameter using `NSFullSizeContentViewWindowMask` for rounded corners
  - `fix-window-role.patch`: changes NSAccessibility role from TextField to Window (needed for tiling WMs)
  - `adjust-ns-init-colors.patch` (unstable only): moves `ns_init_colors()` after `init_callproc()`
- Custom 3D `.icns` icon replacement in the .app bundle
- Platform restricted to Darwin only
- Defines emacs-unstable, emacs-30, emacs-29

#### Design Decisions

**Approach**: Use `pkgs.emacs30.override` + `.overrideAttrs` rather than duplicating `make-emacs.nix` from scratch. This:
- Leverages all existing nixpkgs build logic (configure flags, dependencies, patches, postInstall)
- Stays maintainable as nixpkgs updates
- Preserves binary cache hits for the default mainline build

**Architecture**:
- All 30+ boolean flags exposed as top-level function arguments with defaults matching nixpkgs
- `variant` argument selects source: mainline (tarball), git (master), unstable (latest tag), macport (fork), igc (GC branch)
- `rev`/`hash` arguments allow pinning specific commits for git-based variants
- Base package selection: `pkgs.emacs30-macport` for macport variant, `pkgs.emacs30` for everything else
- `needsOverride` guard: skip `overrideAttrs` entirely for plain mainline builds to preserve the same derivation hash as `pkgs.emacs30` (binary cache)
- Darwin patches opt-in (default false) with `fetchpatch` from nix-giant/nix-darwin-emacs; hashes use `lib.fakeHash` — nix reports the correct hash on first build
- aarch64-linux `--enable-check-lisp-object-type` fix from emacs-overlay applied in overrideAttrs
- IGC variant adds `pkgs.mps` to buildInputs and `--with-mps=yes` to configureFlags

**Lazy evaluation considerations**:
- Function argument defaults reference other arguments (e.g., `withGTK3 ? (withPgtk && !noGui)`) — works because Nix attribute set defaults are lazily evaluated
- `null` sentinels for `rev`/`hash` — git sources only fetched when the variant is actually selected
- `lib.optionalAttrs` used to conditionally include source overrides without creating unnecessary attrset merges

**Edge cases handled**:
- macport with custom `rev` — uses `fetchFromGitHub` from jdtsmith/emacs-mac
- `withNS` correctly defaults to false for macport variant
- `withFixWindowRolePatch` only applied for Emacs 30 (already fixed upstream for 31+)
- `patchBranch` auto-selects patch directory: "unstable" for git/igc, "30" for mainline/unstable/macport

#### Result

Wrote `emacs.nix` — 185 lines. Verified:
- `nix-instantiate --parse emacs.nix` — parses OK
- `nix-instantiate emacs.nix` — evaluates to the same `.drv` as `pkgs.emacs30` (binary-cached)

### Prompt 2

> Append all my prompts and your process and thought and thinking to JOURNAL.md

Appended this journal entry documenting all prompts and the full research/design/implementation process.

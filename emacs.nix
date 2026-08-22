# emacs.nix — Build GNU Emacs from source with all build options exposed.
#
# This file builds a bare Emacs binary. For the full distribution with
# tree-sitter grammars, use default.nix instead.
#
# THE BUILD MATRIX is deliberately narrow — four shipped builds, on
# {x86_64, aarch64} × {Linux, Darwin}:
#   * Linux GUI      — pgtk (pure GTK / Wayland). The ONLY Linux GUI;
#                      X11/Lucid/GTK3-x11/Motif/Athena are not supported
#                      and asserted away below.
#   * Linux terminal — noGui build.
#   * Darwin GUI     — NS/Cocoa with the nix-giant patches applied by
#                      default (system-appearance, round-undecorated-
#                      frame; adjust-ns-init-colors where the branch has
#                      it). Always a from-source build — the patches put
#                      it off every binary cache by design.
#   * Darwin terminal — noGui build.
#
# Base packages per variant:
#   * mainline — pkgs.emacs from nixpkgs (default Emacs attr,
#     Hydra-cached; kept as the cache-parity canary and escape hatch)
#   * git/unstable/igc — pkgs.emacs-git / emacs-unstable / emacs-igc from
#     nix-community/emacs-overlay (cached on nix-community.cachix.org)
# The overlay is wired up in flake.nix and devenv.nix; this file also
# re-applies it when called standalone via `nix-build emacs.nix` (the
# overlays attr is derived from flake.lock).
#
# Usage:
#   nix-build emacs.nix                                            # unstable variant (Emacs 31 pretest);
#                                                                  #   pgtk GUI on Linux, NS+patches on Darwin
#   nix-build emacs.nix --arg noGui true                           # terminal-only
#   nix-build emacs.nix --arg withNativeCompilation false          # disable native-comp
#   nix-build emacs.nix --arg variant '"git"'                      # bleeding-edge master
#   nix-build emacs.nix --arg variant '"mainline"'                 # nixpkgs default emacs attr
#   nix-build emacs.nix --arg variant '"igc"'                      # incremental GC branch
#
# git/unstable/igc track the revision pinned by the overlay and are
# binary-cache hits on Linux. Only when pinning a custom commit via
# --argstr rev does the first build fail and report the hash to pass
# back:
#   nix-build emacs.nix --arg variant '"git"' --argstr rev "abc123..." --argstr hash "sha256-..."
#
# igc on Darwin has no prebuilt upstream (plan.md §3), so it is a
# from-source build there even at the default rev. For that case, and
# for any custom-rev/patched build above, --arg useCcache true swaps in
# pkgs.ccacheStdenv to make repeat local rebuilds cheaper — see its doc
# comment below for the (required, one-time, machine-level) ccache
# setup. It is a no-op improvement, not a cache-parity path: never turn
# it on for a plain default-rev build that would otherwise hit the
# binary cache.
#
# Adapted from the `next` branch. Source for nixpkgs is the flake.lock-pinned
# nixpkgs-unstable channel; pass --arg pkgs '<nixpkgs>' or override `pkgs`
# at the command line to use a different one.
#
# Based on:
#   - NixOS/nixpkgs       pkgs/applications/editors/emacs/ (make-emacs.nix)
#   - nix-community       github:nix-community/emacs-overlay
#   - nix-giant           github:nix-giant/nix-darwin-emacs
{
  system ? builtins.currentSystem,
  pkgs ?
    import
      (
        let
          lock = builtins.fromJSON (builtins.readFile ./flake.lock);
          # x86_64-darwin was dropped from nixpkgs-unstable (26.11); the flake
          # pins it to nixpkgs-26.05-darwin via a dedicated input, so bare
          # emacs.nix / variant builds on that platform must read the same node.
          nixpkgsNode =
            if system == "x86_64-darwin" then
              lock.nodes.root.inputs.nixpkgs-x86_64-darwin
            else
              lock.nodes.root.inputs.nixpkgs;
          n = lock.nodes.${nixpkgsNode}.locked;
        in
        fetchTarball {
          url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz";
          sha256 = n.narHash;
        }
      )
      {
        inherit system;
        overlays = [
          (import (
            let
              lock = builtins.fromJSON (builtins.readFile ./flake.lock);
              n = lock.nodes.emacs-overlay.locked;
            in
            fetchTarball {
              url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz";
              sha256 = n.narHash;
            }
          ))
        ];
      },

  # Source variant
  #   "unstable"  — Emacs 31 release branch (currently the 31.0.90
  #                 pretest); the default here and for the distribution
  #                 (mk-overlay.nix passes variant = "unstable")
  #   "git"       — bleeding-edge master from git.savannah.gnu.org
  #   "igc"       — feature/igc3 incremental garbage collector branch
  #   "mainline"  — nixpkgs default emacs attr (Hydra-cached; the
  #                 cache-parity canary, not a shipped build)
  variant ? "unstable",

  # Source overrides — pin a specific commit for git/unstable/igc
  # instead of the overlay pin:
  #   --argstr rev "abc123..." --argstr hash "sha256-..."
  rev ? null,
  hash ? null,

  # Build with pkgs.ccacheStdenv instead of the default stdenv. This is
  # ONLY useful — and only meant to be turned on — for a build that is
  # already off the cache-parity path documented below: a custom `rev`,
  # the Darwin GUI build (patched by default, so from-source by design),
  # or the `igc` variant on Darwin (which nix-community.cachix.org does
  # not carry a prebuilt for, so `just build-igc` is a from-source build
  # on that platform even at the default rev — see plan.md §3). Swapping
  # stdenv changes the derivation hash unconditionally, so this flag must
  # stay `false` for every default-rev, no-patch build: turning it on for
  # e.g. plain `variant = "unstable"` on Linux would trade an existing
  # binary-cache hit for a slower, ALSO-uncached local build, the
  # opposite of the point.
  #
  # Requires one piece of machine setup before this has any effect: the
  # cache directory must be writable inside the build sandbox, e.g. in
  # nix.conf: `extra-sandbox-paths = /var/cache/ccache` (and the dir
  # created writable by the build users). The CCACHE_DIR half is wired
  # below via ccacheStdenv's `extraConfig` — a bare `pkgs.ccacheStdenv`
  # would default to $HOME/.ccache, which is /homeless-shelter inside
  # the sandbox and can never hit; environment variables set on the
  # machine do not reach sandboxed builders. Without the sandbox
  # exception the build still succeeds (from source) — ccache just
  # misses every time.
  useCcache ? false,
  # Where ccache keeps its cache (must match the extra-sandbox-paths
  # entry above). Only read when useCcache = true.
  ccacheDir ? "/var/cache/ccache",

  # GUI toolkit
  # The matrix admits exactly one GUI per platform: pgtk on Linux, NS on
  # Darwin. The X11 toolkits (Lucid/GTK3-x11/Motif/Athena) are gone —
  # not arguments any more, and asserted unreachable below. GTK3 the
  # *library* is still a build input of the pgtk build (make-emacs.nix's
  # own withGTK3 default is `withPgtk && !noGui`, which we leave to the
  # base); what was dropped is GTK3-as-X11-toolkit.
  noGui ? false, # terminal only (--without-x --without-ns)
  withPgtk ? (pkgs.stdenv.hostPlatform.isLinux && !noGui),
  # --with-pgtk (pure GTK / Wayland; the Linux GUI)
  withNS ? (pkgs.stdenv.hostPlatform.isDarwin && !noGui),
  # Cocoa/NeXTstep (macOS native GUI)
  withXwidgets ? null,
  # --with-xwidgets (embedded webkit widgets). `null` (the default)
  # means "whatever the base package was built with" — xwidgets off for
  # the overlay's git/unstable/igc attrs, i.e. off everywhere in this
  # matrix — and forwards nothing, so cache parity cannot be disturbed.
  # An explicit bool is forwarded and (when it differs from the base)
  # intentionally busts the cache, like a rev pin. This replaces the old
  # mirrored-default scheme, whose mirror expression could drift from
  # make-emacs.nix's own version-conditional default.

  # Compilation
  withNativeCompilation ? (pkgs.stdenv.buildPlatform.canExecute pkgs.stdenv.hostPlatform),
  # --with-native-compilation (libgccjit AOT)
  withCompressInstall ? true, # --with-compress-install (gzip .el files)
  withCsrc ? true, # install C sources for find-function-C-source
  # Every base is a git checkout these days: nixpkgs fetches emacs via
  # fetchgit (srcRepo defaults to true in make-emacs.nix and nixpkgs
  # does not override it), and emacs-overlay passes srcRepo = true for
  # git/unstable/igc.
  srcRepo ? true,
  # source is a git checkout (runs autoreconf)

  # Image formats
  withWebP ? true,
  withImageMagick ? false, # --with-imagemagick (off by default since Emacs 27)

  # Libraries & features
  withTreeSitter ? true,
  withSQLite3 ? true,
  withDbus ? pkgs.stdenv.hostPlatform.isLinux,
  withSelinux ? pkgs.stdenv.hostPlatform.isLinux,
  withGpm ? pkgs.stdenv.hostPlatform.isLinux,
  # --with-gpm (mouse in terminal, Linux)
  withAlsaLib ? false, # ALSA sound (Linux)
  withAcl ? false, # POSIX ACL support (Linux)
  withMailutils ? true, # --with-mailutils (GNU Mailutils movemail)
  withSystemd ? pkgs.lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.systemdLibs,
  # --with-systemd (journal support)
  withSmallJaDic ? false,
  withGcMarkTrace ? false, # --with-gc-mark-trace (experimental in Emacs 30)
  withGlibNetworking ? withPgtk,
  # GLib networking / TLS for GIO. make-emacs.nix's own default is
  # `withPgtk || withGTK3 || (withX && withXwidgets)`; with the X11
  # toolkits gone and withGTK3 left to the base (whose default is
  # `withPgtk && !noGui`), the reachable part reduces to withPgtk.

  # macOS patches (from nix-giant/nix-darwin-emacs)
  # Originally from d12frosted/homebrew-emacs-plus. Only applied on
  # Darwin, and ON BY DEFAULT for the NS GUI build — the matrix's Darwin
  # GUI is "modern macOS, patched", accepting that this puts every
  # Darwin GUI build permanently off the binary caches (pair with
  # useCcache for iteration). The noGui Darwin build takes none of them
  # (withNS is false there), so it stays unpatched. Fetched from the
  # commit pinned in darwinPatchesRev below; hashes are pinned per patch
  # branch in darwinPatchHashes. Updating means bumping the rev — a
  # patch rewritten upstream then reports its new hash at build time.
  withSystemAppearancePatch ? withNS,
  # Adds ns-system-appearance variable and
  # ns-system-appearance-change-functions hook for Dark/Light mode detection
  withRoundUndecoratedFramePatch ? withNS,
  # Adds `undecorated-round` frame parameter for rounded-corner
  # borderless windows using NSFullSizeContentViewWindowMask
  withAdjustNsInitColorsPatch ? withNS,
  # Moves ns_init_colors() after init_lread() so data-directory is set
  # when colors load (full palette instead of the ~62 dump-time colors).
  # Only exists in patches-unstable — silently skipped (see
  # darwinPatches below) unless the variant maps to that branch.
}:

# The build matrix: the only GUIs are pgtk on Linux and NS on Darwin.
# A Linux build must be pgtk or terminal-only; a Darwin build NS or
# terminal-only. This is what makes the removed X11/Lucid/Motif/Athena
# escape hatches unreachable rather than merely undocumented.
assert pkgs.stdenv.hostPlatform.isLinux -> (noGui || withPgtk);
assert pkgs.stdenv.hostPlatform.isDarwin -> (noGui || withNS);
assert builtins.elem variant [
  "mainline"
  "git"
  "unstable"
  "igc"
];

let
  inherit (pkgs)
    lib
    stdenv
    fetchgit
    fetchpatch
    ;

  inherit (stdenv.hostPlatform) isDarwin;
  isGitVariant = builtins.elem variant [
    "git"
    "unstable"
    "igc"
  ];

  # Git-based source metadata
  # Only used when a custom rev is pinned via --argstr rev. Without it,
  # git/unstable/igc build the revision already pinned (with a real
  # hash) inside emacs-overlay, so no hash dance is needed.
  gitBranch = {
    git = "master";
    unstable = "emacs-31";
    igc = "feature/igc3";
  };

  customGitSrc = fetchgit {
    url = "https://git.savannah.gnu.org/git/emacs.git";
    inherit rev;
    hash = if hash != null then hash else lib.fakeHash;
  };

  # Base package per variant
  #   * mainline — nixpkgs default emacs attr (Hydra-cached)
  #   * git/unstable/igc — emacs-overlay's prebuilt attrs (cached on
  #     nix-community.cachix.org; emacs-igc already carries
  #     --with-mps=yes and the mps buildInput)
  #
  # When withPgtk is requested (the Linux GUI default), select the
  # prebuilt `*-pgtk` sibling (emacs-overlay for git/unstable/igc,
  # nixpkgs for mainline) instead of overriding withPgtk on the non-pgtk
  # base. Both build byte-identical *content*, but the sibling carries a
  # distinct derivation `name` (`…-pgtk-…`) that feeds the output-path
  # hash — so a plain `emacs-unstable.override { withPgtk = true; }`
  # lands on a *different* store path and misses the binary cache,
  # rebuilding the same bytes from source. Starting from the sibling
  # preserves cache parity: the args this file forwards match how
  # emacs-overlay built the sibling, so the `basePackage.override` below
  # is a no-op. `or`-guarded so a downstream consumer on an older
  # nixpkgs that lacks a given sibling falls back to overriding the
  # non-pgtk base (correct, from source).
  # The git/unstable/igc base attrs exist ONLY when nix-community/emacs-
  # overlay is composed into `pkgs`. Prefer the prebuilt `-pgtk` sibling
  # (with the `or` fallback documented above), but if even the non-pgtk
  # base is absent — an overlay-less `pkgs` — throw a message pointing at
  # overlays.default / variant = "mainline" instead of Nix's opaque
  # "attribute 'emacs-unstable' missing". The throw is fully lazy: on the
  # cached happy path the sibling resolves and `base` is never forced, so
  # cache parity is untouched.
  overlayBase =
    {
      pgtkAttr,
      baseAttr,
    }:
    let
      base =
        pkgs.${baseAttr} or (throw (
          "jotain emacs.nix: variant \"${variant}\" needs nix-community/emacs-overlay "
          + "composed into `pkgs` (this flake's overlays.default provides `${baseAttr}`). "
          + "For stock nixpkgs Emacs use variant = \"mainline\"."
        ));
    in
    if withPgtk then pkgs.${pgtkAttr} or base else base;

  basePackage =
    if variant == "git" then
      overlayBase {
        pgtkAttr = "emacs-git-pgtk";
        baseAttr = "emacs-git";
      }
    else if variant == "unstable" then
      overlayBase {
        pgtkAttr = "emacs-unstable-pgtk";
        baseAttr = "emacs-unstable";
      }
    else if variant == "igc" then
      overlayBase {
        pgtkAttr = "emacs-igc-pgtk";
        baseAttr = "emacs-igc";
      }
    else
      (if withPgtk then pkgs.emacs-pgtk or pkgs.emacs else pkgs.emacs);

  # Forward all boolean flags to make-emacs.nix
  #
  # CACHE-PARITY INVARIANT (Linux and noGui builds): every default in
  # this file's argument list must match the corresponding default in
  # upstream nixpkgs make-emacs.nix (and the explicit args emacs-overlay
  # passes for its git/unstable/igc attrs). When that holds, calling
  #
  #     import ./emacs.nix {}                  # unstable; pgtk on Linux
  #     import ./emacs.nix { noGui = true; }   # terminal-only
  #
  # produces on Linux the *exact* store path of the matching prebuilt
  # attr (emacs-unstable-pgtk for the default; pkgs.emacs-pgtk for
  # variant "mainline") — so binary caches (Hydra,
  # nix-community.cachix.org, jylhis) hit and Linux never rebuilds Emacs
  # from source. Expected divergences: custom rev pins, an explicit
  # `useCcache = true`, and — BY DESIGN — every Darwin GUI build, whose
  # default-on patches run through `overrideAttrs` below and put it
  # permanently off the caches. Darwin noGui stays a pure override.
  #
  # Verify after any change to defaults (on Linux; also with variant
  # '"git"' vs pkgs.emacs-git-pgtk, '"igc"' vs pkgs.emacs-igc-pgtk, and
  # noGui = true vs the non-pgtk base override):
  #     nix-instantiate --eval --strict -E \
  #       'let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  #            nixpkgsNode = lock.nodes.root.inputs.nixpkgs;
  #            n = lock.nodes.${nixpkgsNode}.locked;
  #            ov = lock.nodes.emacs-overlay.locked;
  #            nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
  #            overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
  #            pkgs = import nixpkgs { overlays = [ (import overlay) ]; };
  #        in {
  #             default = (import ./emacs.nix {}).outPath == pkgs.emacs-unstable-pgtk.outPath;
  #             mainline-pgtk = (import ./emacs.nix { variant = "mainline"; }).outPath == pkgs.emacs-pgtk.outPath;
  #           }'
  #
  # nixpkgs-version-portable override
  #
  # `make-emacs.nix` has grown arguments over nixpkgs releases. Passing
  # an argument the base derivation does not define makes `.override`
  # throw "called with unexpected argument", which would break the flake
  # for a downstream consumer that overrides nixpkgs with an older
  # release (24.05+) — notably via the Home Manager / NixOS / nix-on-droid
  # modules, which apply this overlay to the *consumer's* pkgs. We
  # therefore filter the override set down to the arguments the base
  # actually accepts, discovered from `lib.functionArgs
  # basePackage.override` (the make-emacs formals carry through
  # `makeOverridable`). On the pinned nixpkgs-unstable every argument is
  # accepted, so the intersection is a no-op and the cache-parity
  # invariant documented above is preserved exactly; on 24.05 the
  # not-yet-existing flags (noGui, srcRepo, withGcMarkTrace, …) are
  # dropped, and terminal/GUI selection still flows through the explicit
  # `with*` flags that have existed all along.
  overrideArgs = {
    inherit
      noGui
      srcRepo
      withPgtk
      withNS
      withNativeCompilation
      withCompressInstall
      withCsrc
      withTreeSitter
      withSQLite3
      withWebP
      withImageMagick
      withDbus
      withSelinux
      withGpm
      withAlsaLib
      withAcl
      withMailutils
      withSystemd
      withSmallJaDic
      withGcMarkTrace
      withGlibNetworking
      ;
  }
  # The removed X11-era arguments (withX, withGTK3, withMotif, withAthena,
  # withCairo, withXinput2, withToolkitScrollBars) are deliberately NOT
  # forwarded: the base package's own defaults for them are exactly what
  # the cached artifacts were built with, so not passing them is
  # parity-neutral — and there is no supported configuration in this
  # matrix that would set them to anything else.
  #
  # withXwidgets: null means "follow the base" and forwards nothing (see
  # the argument's doc comment); an explicit bool is forwarded and may
  # intentionally bust the cache.
  // lib.optionalAttrs (withXwidgets != null) {
    inherit withXwidgets;
  }
  # See the `useCcache ?` doc comment above: default false forwards
  # nothing here, so the default path is byte-for-byte what it was
  # before this flag existed. The extraConfig override is what points
  # ccache at the sandbox-visible cache dir — bare ccacheStdenv would
  # write to $HOME/.ccache (= /homeless-shelter in the sandbox) and
  # never hit.
  // lib.optionalAttrs useCcache {
    stdenv = pkgs.ccacheStdenv.override {
      extraConfig = ''
        export CCACHE_DIR=${ccacheDir}
        export CCACHE_UMASK=007
      '';
    };
  };

  overridden = basePackage.override (
    lib.intersectAttrs (lib.functionArgs basePackage.override) overrideArgs
  );

  # Darwin patches (fetched from nix-giant/nix-darwin-emacs)
  # Patch directory: "unstable" for master/32+; otherwise keyed on what
  # the base package actually is, not on the variant name — "30" for
  # Emacs 30.x (mainline while nixpkgs' default attr is 30.x), "31" for
  # Emacs 31.x (unstable, and mainline once nixpkgs promotes 31 to
  # pkgs.emacs).
  patchBranch =
    if variant == "git" || variant == "igc" then
      "unstable"
    else if lib.versionOlder basePackage.version "31" then
      "30"
    else
      "31";

  # fetchpatch output hashes per patch branch. The 31 and unstable
  # branches currently carry identical patch content; 30 differs.
  darwinPatchHashes = {
    "31" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-KCMEvJzN1OkwFYoMLpZghvdeoO1Ckcxk3Mo19YAf850=";
    };
    "unstable" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-KCMEvJzN1OkwFYoMLpZghvdeoO1Ckcxk3Mo19YAf850=";
      "adjust-ns-init-colors.patch" = "sha256-Pqq1FA7caSLk4R5YsKKqh5WzttQ2BWGvAvKAEaOZIJI=";
    };
    "30" = {
      "system-appearance.patch" = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
      "round-undecorated-frame.patch" = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
    };
  };

  # Pinned nix-giant/nix-darwin-emacs commit the patches are fetched
  # from. Branch URLs are mutable — an upstream rewrite/move/delete on
  # `main` would make these fixed-output fetches fail (hash mismatch or
  # 404) at an arbitrary future date on machines without the store
  # paths. Bumping this rev is the deliberate update path; if the patch
  # content changed upstream, the build reports the new hashes for the
  # table above.
  darwinPatchesRev = "c54ec432033e0e099ba9d4b4da8a1667971b3134"; # main as of 2026-07-21

  darwinPatch =
    name:
    fetchpatch {
      inherit name;
      url =
        "https://raw.githubusercontent.com/nix-giant/nix-darwin-emacs"
        + "/${darwinPatchesRev}/overlays/patches-${patchBranch}/${name}";
      hash = darwinPatchHashes.${patchBranch}.${name};
    };

  darwinPatches =
    lib.optional (isDarwin && withSystemAppearancePatch) (darwinPatch "system-appearance.patch")
    ++ lib.optional (isDarwin && withRoundUndecoratedFramePatch) (
      darwinPatch "round-undecorated-frame.patch"
    )
    # Only exists in patches-unstable (master/32+); upstream has not
    # backported it to the 31/30 branches.
    ++ lib.optional (isDarwin && withAdjustNsInitColorsPatch && patchBranch == "unstable") (
      darwinPatch "adjust-ns-init-colors.patch"
    );

  # Determine if overrideAttrs is needed
  # Skip overrideAttrs unless a custom rev is pinned or Darwin patches
  # apply (they do by default on the Darwin NS GUI) — every default-rev
  # Linux/noGui variant is then a pure basePackage.override and stays a
  # binary cache hit.
  hasCustomGitSrc = isGitVariant && rev != null;
  needsOverride = hasCustomGitSrc || darwinPatches != [ ];

in
if !needsOverride then
  overridden
else
  overridden.overrideAttrs (
    old:
    # Source override for git-based variants pinned to a custom rev.
    # The overlay's --enable-check-lisp-object-type (aarch64-linux) and
    # emacs-igc's --with-mps=yes / mps buildInput survive in
    # old.configureFlags / old.buildInputs, so nothing is re-added here.
    lib.optionalAttrs hasCustomGitSrc {
      src = customGitSrc;
    }
    // {
      patches = (old.patches or [ ]) ++ darwinPatches;

      # Embed git revision so emacs-repository-get-version works
      # without a .git directory in the build tree. (The base package
      # already substituted its own pinned rev; --replace-warn keeps
      # the second pass non-fatal.)
      postPatch =
        (old.postPatch or "")
        + lib.optionalString hasCustomGitSrc ''
          substituteInPlace lisp/loadup.el \
            --replace-warn '(emacs-repository-get-version)' '"${rev}"' \
            --replace-warn '(emacs-repository-get-branch)' '"${gitBranch.${variant}}"'
        '';
    }
  )

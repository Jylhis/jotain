# emacs.nix: bare GNU Emacs with its build options exposed. For the full
# distribution (packages + tree-sitter grammars), use default.nix.
#
# THE BUILD MATRIX: four builds on {x86_64, aarch64}:
#   * Linux GUI:       pgtk (GTK / Wayland), the only Linux GUI. X11,
#                      Lucid, GTK3-x11, Motif and Athena are asserted away.
#   * Linux terminal:  noGui.
#   * Darwin GUI:      NS/Cocoa with the macOS patches on by default
#                      (system-appearance, round-undecorated-frame,
#                      fix-ns-x-colors), so always built from source.
#   * Darwin terminal: noGui.
#
# Called standalone (`nix-build emacs.nix`), the default `pkgs` applies
# emacs-overlay pinned from flake.lock.
#
# Usage:
#   nix-build emacs.nix                                            # unstable variant (Emacs 31.1);
#                                                                  #   pgtk GUI on Linux, NS+patches on Darwin
#   nix-build emacs.nix --arg noGui true                           # terminal-only
#   nix-build emacs.nix --arg withNativeCompilation false          # disable native-comp
#   nix-build emacs.nix --arg variant '"git"'                      # bleeding-edge master
#   nix-build emacs.nix --arg variant '"mainline"'                 # nixpkgs default emacs attr
#   nix-build emacs.nix --arg variant '"igc"'                      # incremental GC branch
#   nix-build emacs.nix --arg cpuTune '"icelake-client"'           # CPU-tuned perf build
#                                                                  #   (opt-in; off every binary cache)
#
# git/unstable/igc build the overlay-pinned revision (cache hits on
# Linux). A custom --argstr rev fails once and reports the hash to pass
# back:
#   nix-build emacs.nix --arg variant '"git"' --argstr rev "abc123..." --argstr hash "sha256-..."
#
# nixpkgs is the flake.lock pin; pass --arg pkgs '<nixpkgs>' to override.
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
          # x86_64-darwin left nixpkgs-unstable (26.11); read the flake's
          # nixpkgs-26.05-darwin input instead.
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
  #   "unstable"  — the newest Emacs release or pretest tag (currently
  #                 31.1); the default here and for the distribution
  #                 (nix/mk-overlay.nix)
  #   "git"       — bleeding-edge master from git.savannah.gnu.org
  #   "igc"       — feature/igc3 incremental garbage collector branch
  #   "mainline"  — nixpkgs default emacs attr (Hydra-cached; the
  #                 cache-parity canary, not a shipped build)
  variant ? "unstable",

  # Source overrides — pin a custom commit instead of the overlay pin.
  rev ? null,
  hash ? null,

  # Build with pkgs.ccacheStdenv. Swapping stdenv always changes the
  # derivation hash, so only enable this for builds already off the cache
  # (a custom `rev`, the Darwin GUI, or `igc` on Darwin, which
  # nix-community.cachix.org does not carry; see TODO.md §3). On a cached
  # build it trades a cache hit for an uncached local build.
  #
  # Needs the cache dir writable in the sandbox, e.g. nix.conf
  # `extra-sandbox-paths = /var/cache/ccache` (dir writable by the build
  # users). CCACHE_DIR is set below via `extraConfig`: host env vars do not
  # reach sandboxed builders, and the default $HOME/.ccache is
  # /homeless-shelter there. Without the sandbox path the build still
  # succeeds, but ccache always misses.
  useCcache ? false,
  # Must match the extra-sandbox-paths entry. Only read when useCcache.
  ccacheDir ? "/var/cache/ccache",

  # CPU-tuned perf build (opt-in). `null` forwards nothing. A string
  # (e.g. "icelake-client") appends `-O3 -march=<cpuTune>
  # -mtune=<cpuTune>` to NIX_CFLAGS_COMPILE, which, like `useCcache`,
  # puts the build off every binary cache.
  cpuTune ? null,

  # GUI toolkit: pgtk on Linux, NS on Darwin. GTK3 the library is still a
  # pgtk build input (make-emacs.nix defaults withGTK3 to
  # `withPgtk && !noGui`, left to the base).
  noGui ? false, # terminal only (--without-x --without-ns)
  withPgtk ? (pkgs.stdenv.hostPlatform.isLinux && !noGui),
  # --with-pgtk (pure GTK / Wayland; the Linux GUI)
  withNS ? (pkgs.stdenv.hostPlatform.isDarwin && !noGui),
  # Cocoa/NeXTstep (macOS native GUI)
  withXwidgets ? null,
  # --with-xwidgets. `null` forwards nothing and follows the base package
  # (off everywhere in this matrix), so it can never drift from
  # make-emacs.nix's version-conditional default. An explicit bool is
  # forwarded and, if it differs from the base, busts the cache.

  # Compilation
  withNativeCompilation ? (pkgs.stdenv.buildPlatform.canExecute pkgs.stdenv.hostPlatform),
  # --with-native-compilation (libgccjit AOT)
  withCompressInstall ? true, # --with-compress-install (gzip .el files)
  withCsrc ? true, # install C sources for find-function-C-source
  # Source is a git checkout (runs autoreconf). True for every base:
  # make-emacs.nix defaults it and emacs-overlay passes it.
  srcRepo ? true,

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
  # GLib networking / TLS for GIO. make-emacs.nix's default
  # `withPgtk || withGTK3 || (withX && withXwidgets)` reduces to withPgtk
  # in this matrix.

  # macOS patches (d12frosted/homebrew-emacs-plus, via
  # nix-giant/nix-darwin-emacs for the 30/31 branches). On by default
  # for the NS GUI only, which keeps every Darwin GUI build off the
  # binary caches (pair with useCcache when iterating). Darwin noGui
  # stays unpatched. Sources and hashes: see patchBranch below.
  withSystemAppearancePatch ? withNS,
  # Adds ns-system-appearance variable and
  # ns-system-appearance-change-functions hook for Dark/Light mode detection
  withRoundUndecoratedFramePatch ? withNS,
  # Adds `undecorated-round` frame parameter for rounded-corner
  # borderless windows using NSFullSizeContentViewWindowMask
  withFixNsXColorsPatch ? withNS,
  # Refreshes x-colors from ns-list-colors at NS window-system init, so
  # the runtime palette is the full ~800-color list instead of the ~62
  # colors captured into the pdump headlessly.
}:

# The build matrix: the only GUIs are pgtk on Linux and NS on Darwin.
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

  # Only used with a custom --argstr rev; otherwise git/unstable/igc
  # build the revision emacs-overlay pins.
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

  # Base package per variant: nixpkgs' emacs for mainline, the
  # emacs-overlay prebuilts for git/unstable/igc (cached on
  # nix-community.cachix.org).
  #
  # With withPgtk (the Linux GUI default), select the prebuilt `*-pgtk`
  # sibling rather than overriding withPgtk on the non-pgtk base. The
  # content is byte-identical, but the sibling's derivation `name`
  # (`…-pgtk-…`) feeds the output-path hash, so
  # `emacs-unstable.override { withPgtk = true; }` lands on a different
  # store path and misses the cache. From the sibling, the forwarded args
  # match how it was built, so the override below is a no-op. The `or`
  # falls back to overriding the non-pgtk base (from source) on an older
  # nixpkgs that lacks the sibling.
  #
  # git/unstable/igc exist only with emacs-overlay composed into `pkgs`;
  # without it, throw a readable error instead of "attribute
  # 'emacs-unstable' missing". The throw is lazy: on the happy path
  # `base` is never forced.
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
  # variant "mainline"), so binary caches (Hydra,
  # nix-community.cachix.org, jylhis) hit and Linux never rebuilds Emacs
  # from source. Expected divergences: custom rev pins, `useCcache`,
  # `cpuTune`, and, by design, every Darwin GUI build (its default-on
  # patches go through `overrideAttrs` below). Darwin noGui stays a pure
  # override.
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
  # make-emacs.nix has grown arguments over nixpkgs releases, and
  # `.override` throws on an unknown one. The modules apply this overlay
  # to the consumer's pkgs, which may be an older nixpkgs (24.05+), so
  # the override set is filtered to the arguments the base accepts
  # (`lib.functionArgs basePackage.override`). On the pinned nixpkgs
  # every argument is accepted, so the filter is a no-op and parity
  # holds; on 24.05 newer flags (noGui, srcRepo, withGcMarkTrace, …) are
  # dropped and GUI selection still flows through the older `with*` flags.
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
  # The X11-era arguments (withX, withGTK3, withMotif, withAthena,
  # withCairo, withXinput2, withToolkitScrollBars) are deliberately not
  # forwarded: the base defaults are what the cached artifacts were built
  # with, and no supported configuration changes them.
  // lib.optionalAttrs (withXwidgets != null) {
    inherit withXwidgets;
  }
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

  # Darwin patches (macOS GUI only)
  # Patch branch: "unstable" for master/32+ (git/igc); otherwise keyed on
  # the base package's version, not the variant name: "30" for Emacs 30.x,
  # "31" for 31.x (unstable, and mainline once nixpkgs moves to 31).
  #
  # 30/31 come from nix-giant; "unstable" comes straight from
  # d12frosted/homebrew-emacs-plus, because nix-giant dropped its
  # patches-unstable branch (2026-08-20).
  patchBranch =
    if variant == "git" || variant == "igc" then
      "unstable"
    else if lib.versionOlder basePackage.version "31" then
      "30"
    else
      "31";

  # fetchpatch output hashes per patch branch (context lines differ per
  # branch). The unstable table reuses two of the 31 hashes: those
  # homebrew files are byte-identical to nix-giant's 31 copies after
  # fetchpatch normalization.
  darwinPatchHashes = {
    "31" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-KCMEvJzN1OkwFYoMLpZghvdeoO1Ckcxk3Mo19YAf850=";
      "fix-ns-x-colors.patch" = "sha256-R1CKmkxLmXmySWUnvNLzwM/H21TJ0tBXDvKPq7bH02c=";
    };
    "30" = {
      "system-appearance.patch" = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
      "round-undecorated-frame.patch" = "sha256-fesZ0H3LO6T2AiRV8ASozKxZBpvVzwLEcLDy6rctR6c=";
      "fix-ns-x-colors.patch" = "sha256-SkNGXsexkHqughSha8q1K0IYhkpMWMH80tq8G/5CZfw=";
    };
    "unstable" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-KCMEvJzN1OkwFYoMLpZghvdeoO1Ckcxk3Mo19YAf850=";
      "fix-ns-x-colors.patch" = "sha256-oe3DFgEXwp0cZJl+ufWqTonaeWSliikTRsVDNbcy4Yw=";
    };
  };

  # Pinned commits, one per source: branch URLs are mutable, so an
  # upstream rewrite would break these fixed-output fetches. To update,
  # bump the rev; changed patches then report their new hashes.
  darwinPatchesRev = "d8dd282e06e28aae5da09d3888f1f5f4db750d36"; # nix-giant main as of 2026-10-04 (31/30 branches)
  homebrewPatchesRev = "3c3863ac20b242ac93d2becb36108b5eee1c8bf4"; # homebrew-emacs-plus master as of 2026-10-06 (unstable branch)

  darwinPatchUrl =
    name:
    if patchBranch == "unstable" then
      # homebrew's patches/emacs-32 entries symlink to ../emacs-31, and
      # raw.githubusercontent serves a symlink's link text, so fetch the
      # emacs-31 target directly.
      "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus"
      + "/${homebrewPatchesRev}/patches/emacs-31/${name}"
    else
      "https://raw.githubusercontent.com/nix-giant/nix-darwin-emacs"
      + "/${darwinPatchesRev}/overlays/patches-${patchBranch}/${name}";

  darwinPatch =
    name:
    fetchpatch {
      inherit name;
      url = darwinPatchUrl name;
      hash = darwinPatchHashes.${patchBranch}.${name};
    };

  darwinPatches =
    lib.optional (isDarwin && withSystemAppearancePatch) (darwinPatch "system-appearance.patch")
    ++ lib.optional (isDarwin && withRoundUndecoratedFramePatch) (
      darwinPatch "round-undecorated-frame.patch"
    )
    ++ lib.optional (isDarwin && withFixNsXColorsPatch) (darwinPatch "fix-ns-x-colors.patch");

  # overrideAttrs only for a custom rev, Darwin patches, or cpuTune, so
  # every default-rev Linux/noGui build stays a pure override (cache hit).
  hasCustomGitSrc = isGitVariant && rev != null;
  needsOverride = hasCustomGitSrc || darwinPatches != [ ] || cpuTune != null;

in
if !needsOverride then
  overridden
else
  overridden.overrideAttrs (
    old:
    # Custom-rev source. The overlay's configure flags and buildInputs
    # (e.g. igc's --with-mps=yes) survive in `old`, so nothing is re-added.
    lib.optionalAttrs hasCustomGitSrc {
      src = customGitSrc;
    }
    // {
      patches = (old.patches or [ ]) ++ darwinPatches;

      # Embed the rev so emacs-repository-get-version works without
      # .git. --replace-warn: the base already substituted its own rev.
      postPatch =
        (old.postPatch or "")
        + lib.optionalString hasCustomGitSrc ''
          substituteInPlace lisp/loadup.el \
            --replace-warn '(emacs-repository-get-version)' '"${rev}"' \
            --replace-warn '(emacs-repository-get-branch)' '"${gitBranch.${variant}}"'
        '';
    }
    // lib.optionalAttrs (cpuTune != null) {
      # Appended: gcc's last flag wins, so -O3 overrides the base -O2.
      NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -O3 -march=${cpuTune} -mtune=${cpuTune}";
    }
  )

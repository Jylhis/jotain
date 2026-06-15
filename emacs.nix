# emacs.nix — Build GNU Emacs from source with all build options exposed.
#
# This file builds a bare Emacs binary. For the full distribution with
# tree-sitter grammars, use default.nix instead.
#
# Base packages per variant:
#   * mainline — pkgs.emacs31 from nixpkgs (release branch, Hydra-cached)
#   * git/unstable/igc — pkgs.emacs-git / emacs-unstable / emacs-igc from
#     nix-community/emacs-overlay (cached on nix-community.cachix.org)
#   * macport — pkgs.emacs-macport, nixpkgs' alias for emacs30-macport
#     (jdtsmith/emacs-mac fork; emacs-overlay does not ship a macport)
# The overlay is wired up in flake.nix and devenv.nix; this file also
# re-applies it when called standalone via `nix-build emacs.nix` (the
# overlays attr is derived from flake.lock).
#
# Usage:
#   nix-build emacs.nix                                            # Emacs 31 release branch (default)
#   nix-build emacs.nix --arg noGui true                           # terminal-only
#   nix-build emacs.nix --arg withPgtk true                        # pure GTK (Wayland)
#   nix-build emacs.nix --arg withGTK3 true                        # GTK3 + X11
#   nix-build emacs.nix --arg withNativeCompilation false          # disable native-comp
#   nix-build emacs.nix --arg variant '"git"'                      # bleeding-edge master
#   nix-build emacs.nix --arg variant '"unstable"'                 # latest release tag
#   nix-build emacs.nix --arg variant '"macport"'                  # macOS macport fork (still Emacs 30)
#   nix-build emacs.nix --arg variant '"igc"'                      # incremental GC branch
#   nix-build emacs.nix --arg withSystemAppearancePatch true       # macOS dark mode hooks
#
# git/unstable/igc/macport track the revision pinned by the overlay (or
# nixpkgs for macport) and are binary-cache hits. Only when pinning a
# custom commit via --argstr rev does the first build fail and report
# the hash to pass back:
#   nix-build emacs.nix --arg variant '"git"' --argstr rev "abc123..." --argstr hash "sha256-..."
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
          nixpkgsNode = lock.nodes.root.inputs.nixpkgs;
          n = lock.nodes.${nixpkgsNode}.locked;
        in
        fetchTarball {
          url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz";
          sha256 = n.narHash;
        }
      )
      {
        inherit system;
        config.allowUnfree = true;
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

  # ── Source variant ────────────────────────────────────────────────
  #   "mainline"  — nixpkgs emacs31 release branch (default, binary-cached)
  #   "git"       — bleeding-edge master from git.savannah.gnu.org
  #   "unstable"  — latest release tag built from git source (srcRepo)
  #   "macport"   — jdtsmith/emacs-mac fork (Darwin only)
  #   "igc"       — feature/igc3 incremental garbage collector branch
  variant ? "mainline",

  # Source overrides — pin a specific commit for git/unstable/igc/macport
  # instead of the overlay/nixpkgs pin:
  #   --argstr rev "abc123..." --argstr hash "sha256-..."
  rev ? null,
  hash ? null,

  # ── GUI toolkit ──────────────────────────────────────────────────
  noGui ? false, # terminal only (--without-x --without-ns)
  withPgtk ? false, # --with-pgtk (pure GTK for Wayland)
  withGTK3 ? (withPgtk && !noGui), # --with-x-toolkit=gtk3
  withMotif ? false, # --with-x-toolkit=motif
  withAthena ? false, # --with-x-toolkit=athena
  withNS ? (pkgs.stdenv.hostPlatform.isDarwin && variant != "macport" && !noGui),
  # Cocoa/NeXTstep (macOS native GUI)
  withX ? !(pkgs.stdenv.hostPlatform.isDarwin || noGui || withPgtk),
  # X11 (Linux default)
  withToolkitScrollBars ? true, # --with-toolkit-scroll-bars
  withXinput2 ? withX, # --with-xinput2 (smooth scrolling on X)
  withXwidgets ? !noGui && (withGTK3 || withPgtk || withNS || variant == "macport"),
  # --with-xwidgets (embedded webkit widgets). Matches the upstream
  # make-emacs.nix default (its extra `isDarwin || major != "30"` clause
  # only matters for non-Darwin Emacs 30, which no jotain variant is).
  # emacs-overlay's git/unstable/igc bases default xwidgets off, but we
  # pass this flag explicitly so our value wins there too.

  # ── Compilation ──────────────────────────────────────────────────
  withNativeCompilation ? (pkgs.stdenv.buildPlatform.canExecute pkgs.stdenv.hostPlatform),
  # --with-native-compilation (libgccjit AOT)
  withCompressInstall ? true, # --with-compress-install (gzip .el files)
  withCsrc ? true, # install C sources for find-function-C-source
  # Every base is a git checkout these days: nixpkgs fetches emacs31 and
  # emacs30-macport via fetchgit/fetchFromGitHub (srcRepo defaults to
  # true in make-emacs.nix and nixpkgs does not override it), and
  # emacs-overlay passes srcRepo = true for git/unstable/igc.
  srcRepo ? true,
  # source is a git checkout (runs autoreconf)

  # ── Image formats ────────────────────────────────────────────────
  withWebP ? true, # --with-webp
  withImageMagick ? false, # --with-imagemagick (off by default since Emacs 27)
  withCairo ? withX, # --with-cairo (2D graphics on X)

  # ── Libraries & features ─────────────────────────────────────────
  withTreeSitter ? true, # --with-tree-sitter
  withSQLite3 ? true, # --with-sqlite3
  withDbus ? pkgs.stdenv.hostPlatform.isLinux,
  # --with-dbus
  withSelinux ? pkgs.stdenv.hostPlatform.isLinux,
  # --with-selinux
  withGpm ? pkgs.stdenv.hostPlatform.isLinux,
  # --with-gpm (mouse in terminal, Linux)
  withAlsaLib ? false, # ALSA sound (Linux)
  withAcl ? false, # POSIX ACL support (Linux)
  withMailutils ? true, # --with-mailutils (GNU Mailutils movemail)
  withSystemd ? pkgs.lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.systemdLibs,
  # --with-systemd (journal support)
  withSmallJaDic ? false, # --with-small-ja-dic
  withGcMarkTrace ? false, # --with-gc-mark-trace (experimental in Emacs 30)
  withGlibNetworking ? (withPgtk || withGTK3 || (withX && withXwidgets)),
  # GLib networking / TLS for GIO

  # ── macOS patches (from nix-giant/nix-darwin-emacs) ──────────────
  # Originally from d12frosted/homebrew-emacs-plus. Only applied on
  # Darwin. Hashes are pinned per patch branch in darwinPatchHashes
  # below; if upstream rewrites a patch, the build reports the new hash.
  withSystemAppearancePatch ? false,
  # Adds ns-system-appearance variable and
  # ns-system-appearance-change-functions hook for Dark/Light mode detection
  withRoundUndecoratedFramePatch ? false,
  # Adds `undecorated-round` frame parameter for rounded-corner
  # borderless windows using NSFullSizeContentViewWindowMask
  withAdjustNsInitColorsPatch ? false,
  # Moves ns_init_colors() after init_lread() so data-directory is set
  # when colors load (full palette instead of the ~62 dump-time colors).
  # Only exists in patches-unstable, i.e. for the git/igc variants.
}:

let
  inherit (pkgs)
    lib
    stdenv
    fetchFromGitHub
    fetchgit
    fetchpatch
    ;

  inherit (stdenv.hostPlatform) isDarwin;
  isGitVariant = builtins.elem variant [
    "git"
    "unstable"
    "igc"
  ];

  # ── Git-based source metadata ────────────────────────────────────
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

  # ── Base package per variant ─────────────────────────────────────
  #   * mainline — nixpkgs emacs31 (release branch, Hydra-cached)
  #   * git/unstable/igc — emacs-overlay's prebuilt attrs (cached on
  #     nix-community.cachix.org; emacs-igc already carries
  #     --with-mps=yes and the mps buildInput)
  #   * macport — nixpkgs emacs-macport alias → emacs30-macport
  #     (jdtsmith/emacs-mac fork, still Emacs 30.2.50 upstream)
  basePackage =
    if variant == "macport" then
      pkgs.emacs-macport
    else if variant == "git" then
      pkgs.emacs-git
    else if variant == "unstable" then
      pkgs.emacs-unstable
    else if variant == "igc" then
      pkgs.emacs-igc
    else
      pkgs.emacs;

  # ── Forward all boolean flags to make-emacs.nix ──────────────────
  #
  # CACHE-PARITY INVARIANT: every default in this file's argument list
  # must match the corresponding default in upstream nixpkgs
  # make-emacs.nix (and the explicit args emacs-overlay passes for its
  # git/unstable/igc attrs). When that holds, calling
  #
  #     import ./emacs.nix {}                  # mainline variant
  #     import ./emacs.nix { noGui = true; }   # any standard override
  #
  # produces the *exact* store path of `pkgs.emacs31(.override { ... })`
  # — and variant "git"/"unstable"/"igc" the store path of the matching
  # emacs-overlay attr — so binary caches (Hydra, nix-community.cachix.org,
  # jylhis) hit and we never rebuild Emacs from source. Only custom rev
  # pins and the Darwin patch flags are expected to diverge — those
  # paths run through `overrideAttrs` below and intentionally bust the
  # cache.
  #
  # Verify after any change to defaults (also with variant '"git"' vs
  # pkgs.emacs-git, '"unstable"' vs pkgs.emacs-unstable, '"igc"' vs
  # pkgs.emacs-igc):
  #     nix-instantiate --eval --strict -E \
  #       'let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  #            nixpkgsNode = lock.nodes.root.inputs.nixpkgs;
  #            n = lock.nodes.${nixpkgsNode}.locked;
  #            ov = lock.nodes.emacs-overlay.locked;
  #            nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
  #            overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
  #            pkgs = import nixpkgs { overlays = [ (import overlay) ]; };
  #        in (import ./emacs.nix {}).outPath == pkgs.emacs31.outPath'
  overridden = basePackage.override {
    inherit
      noGui
      srcRepo
      withPgtk
      withGTK3
      withMotif
      withAthena
      withNS
      withX
      withNativeCompilation
      withCompressInstall
      withCsrc
      withToolkitScrollBars
      withXinput2
      withXwidgets
      withTreeSitter
      withSQLite3
      withWebP
      withImageMagick
      withCairo
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
  };

  # ── Darwin patches (fetched from nix-giant/nix-darwin-emacs) ─────
  # Patch directory: "unstable" for master/32+, "30" for the macport
  # fork (still Emacs 30.x), "31" for Emacs 31.x.
  patchBranch =
    if variant == "git" || variant == "igc" then
      "unstable"
    else if variant == "macport" then
      "30"
    else
      "31";

  # fetchpatch output hashes per patch branch. The 31 and unstable
  # branches currently carry identical patch content; 30 differs.
  darwinPatchHashes = {
    "31" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
    };
    "unstable" = {
      "system-appearance.patch" = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
      "round-undecorated-frame.patch" = "sha256-WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
      "adjust-ns-init-colors.patch" = "sha256-Pqq1FA7caSLk4R5YsKKqh5WzttQ2BWGvAvKAEaOZIJI=";
    };
    "30" = {
      "system-appearance.patch" = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
      "round-undecorated-frame.patch" = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
    };
  };

  darwinPatch =
    name:
    fetchpatch {
      inherit name;
      url =
        "https://raw.githubusercontent.com/nix-giant/nix-darwin-emacs"
        + "/main/overlays/patches-${patchBranch}/${name}";
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

  # ── Macport source override (when rev is provided) ───────────────
  macportSrcOverride = lib.optionalAttrs (variant == "macport" && rev != null) {
    src = fetchFromGitHub {
      owner = "jdtsmith";
      repo = "emacs-mac";
      inherit rev;
      hash = if hash != null then hash else lib.fakeHash;
    };
  };

  # ── Determine if overrideAttrs is needed ─────────────────────────
  # Skip overrideAttrs unless a custom rev is pinned or Darwin patches
  # are requested — every default-rev variant is then a pure
  # basePackage.override and stays a binary cache hit.
  hasCustomGitSrc = isGitVariant && rev != null;
  needsOverride = hasCustomGitSrc || darwinPatches != [ ] || (variant == "macport" && rev != null);

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
    # Source override for macport with custom rev
    // macportSrcOverride
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

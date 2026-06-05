# emacs.nix — Build GNU Emacs from source with all build options exposed.
#
# This file builds a bare Emacs binary. For the full distribution with
# tree-sitter grammars, use default.nix instead.
#
# Emacs 31 is not yet shipped in nixpkgs; the base package comes from
# nix-community/emacs-overlay, which supplies emacs-git (master),
# emacs-igc (feature/igc3), and emacs-macport. The overlay is wired up
# in flake.nix and devenv.nix; this file also re-applies it when called
# standalone via `nix-build emacs.nix` (the overlays attr is derived
# from flake.lock).
#
# Usage:
#   nix-build emacs.nix                                            # Emacs 31 (emacs-git)
#   nix-build emacs.nix --arg noGui true                           # terminal-only
#   nix-build emacs.nix --arg withPgtk true                        # pure GTK (Wayland)
#   nix-build emacs.nix --arg withGTK3 true                        # GTK3 + X11
#   nix-build emacs.nix --arg withNativeCompilation false          # disable native-comp
#   nix-build emacs.nix --arg variant '"git"'                      # bleeding-edge master (alias for default)
#   nix-build emacs.nix --arg variant '"macport"'                  # macOS macport fork (still Emacs 30)
#   nix-build emacs.nix --arg variant '"igc"'                      # incremental GC branch
#   nix-build emacs.nix --arg withSystemAppearancePatch true       # macOS dark mode hooks
#
# For git/unstable/igc variants, the first build will fail and report the
# correct hash. Re-run with:
#   nix-build emacs.nix --arg variant '"git"' --argstr hash "sha256-..."
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
  #   "mainline"  — Emacs 31 release tarball (default, binary-cached)
  #   "git"       — bleeding-edge master from git.savannah.gnu.org
  #   "unstable"  — latest release tag built from git source (srcRepo)
  #   "macport"   — jdtsmith/emacs-mac fork (Darwin only)
  #   "igc"       — feature/igc3 incremental garbage collector branch
  variant ? "mainline",

  # Source overrides — pin a specific commit for git/unstable/igc/macport:
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
  withXwidgets ?
    !noGui
    && (withGTK3 || withPgtk || withNS || variant == "macport")
    && pkgs.stdenv.hostPlatform.isDarwin,
  # --with-xwidgets (embedded webkit widgets)

  # ── Compilation ──────────────────────────────────────────────────
  withNativeCompilation ? (pkgs.stdenv.buildPlatform.canExecute pkgs.stdenv.hostPlatform),
  # --with-native-compilation (libgccjit AOT)
  withCompressInstall ? true, # --with-compress-install (gzip .el files)
  withCsrc ? true, # install C sources for find-function-C-source
  # emacs-overlay's emacs-git / emacs-igc are git checkouts, so they
  # need autoreconfHook (srcRepo = true). emacs-unstable ships the
  # tagged release tarball, so srcRepo = false. macport has its own
  # autotools setup and also wants srcRepo = false.
  srcRepo ? (variant == "git" || variant == "unstable" || variant == "igc" || variant == "mainline"),
  # source is a git checkout (runs autoreconf)

  # ── Image formats ────────────────────────────────────────────────
  withWebP ? true, # --with-webp
  withImageMagick ? false, # --with-imagemagick (off by default since Emacs 27)
  withCairo ? withX, # --with-cairo (2D graphics on X)

  # ── Libraries & features ─────────────────────────────────────────
  withTreeSitter ? true, # --with-tree-sitter
  withSQLite3 ? true, # --with-sqlite3
  withJansson ? false, # --with-jansson (Emacs 30+ has built-in JSON)
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
  # Originally from d12frosted/homebrew-emacs-plus.
  # Only applied on Darwin. On first use the build reports the correct hash
  # to replace lib.fakeHash — update inline or pass via --argstr.
  withSystemAppearancePatch ? false,
  # Adds ns-system-appearance variable and
  # ns-system-appearance-change-functions hook for Dark/Light mode detection
  withRoundUndecoratedFramePatch ? false,
  # Adds `undecorated-round` frame parameter for rounded-corner
  # borderless windows using NSFullSizeContentViewWindowMask
  withFixWindowRolePatch ? false,
  # Fixes NSAccessibility role from NSAccessibilityTextFieldRole to
  # NSAccessibilityWindowRole (needed for tiling WMs like yabai)
  # Only for Emacs 30; already fixed upstream for master (31+)
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
  isAarch64Linux = stdenv.hostPlatform.isLinux && stdenv.hostPlatform.isAarch64;
  isGitVariant = builtins.elem variant [
    "git"
    "unstable"
    "igc"
  ];

  # ── Git-based source metadata ────────────────────────────────────
  # The rev/hash args override these defaults. Branch names resolve at
  # fetch time; hashes must be updated after first build attempt.
  gitMeta = {
    git = {
      srcRev = if rev != null then rev else "master";
      srcHash = if hash != null then hash else lib.fakeHash;
      branch = "master";
    };
    unstable = {
      srcRev = if rev != null then rev else "emacs-31.0.92";
      srcHash = if hash != null then hash else lib.fakeHash;
      branch = "emacs-31";
    };
    igc = {
      srcRev = if rev != null then rev else "feature/igc3";
      srcHash = if hash != null then hash else lib.fakeHash;
      branch = "feature/igc3";
    };
  };

  fetchGitSrc =
    vname:
    fetchgit {
      url = "https://git.savannah.gnu.org/git/emacs.git";
      rev = gitMeta.${vname}.srcRev;
      hash = gitMeta.${vname}.srcHash;
    };

  # ── Base package from emacs-overlay ──────────────────────────────
  # All bases come from nix-community/emacs-overlay (master is Emacs 31).
  #   * emacs-git     — master / Emacs 31 (default mainline build)
  #   * emacs-macport — jdtsmith/emacs-mac fork (still Emacs 30.2.50 upstream)
  basePackage = if variant == "macport" then pkgs.emacs-macport else pkgs.emacs-git;

  # ── Forward all boolean flags to make-emacs.nix ──────────────────
  #
  # CACHE-PARITY INVARIANT: every default in this file's argument list
  # must match the corresponding default in emacs-overlay's
  # emacs-git derivation (which itself mirrors upstream nixpkgs
  # make-emacs.nix). When that holds, calling
  #
  #     import ./emacs.nix {}                  # mainline variant
  #     import ./emacs.nix { noGui = true; }   # any standard override
  #
  # produces the *exact* store path of `pkgs.emacs-git(.override { ... })`
  # so binary caches (nix-community.cachix.org, jylhis) hit and we never
  # rebuild Emacs from source. Only the git/unstable/igc/macport
  # variants and the Darwin patch flags are expected to diverge — those
  # paths run through `overrideAttrs` below and intentionally bust the
  # cache.
  #
  # Verify after any change to defaults:
  #     nix-instantiate --eval --strict -E \
  #       'let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  #            nixpkgsNode = lock.nodes.root.inputs.nixpkgs;
  #            n = lock.nodes.${nixpkgsNode}.locked;
  #            ov = lock.nodes.emacs-overlay.locked;
  #            nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
  #            overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
  #            pkgs = import nixpkgs { overlays = [ (import overlay) ]; };
  #        in (import ./emacs.nix {}).outPath == pkgs.emacs-git.outPath'
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
      withJansson
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
  # Patch directory: "31" for Emacs 31.x, "unstable" for master/32+
  patchBranch = if variant == "git" || variant == "igc" then "unstable" else "31";

  darwinPatchUrl =
    name:
    "https://raw.githubusercontent.com/nix-giant/nix-darwin-emacs"
    + "/main/overlays/patches-${patchBranch}/${name}";

  darwinPatches =
    lib.optional (isDarwin && withSystemAppearancePatch) (fetchpatch {
      name = "system-appearance.patch";
      url = darwinPatchUrl "system-appearance.patch";
      hash = lib.fakeHash;
    })
    ++ lib.optional (isDarwin && withRoundUndecoratedFramePatch) (fetchpatch {
      name = "round-undecorated-frame.patch";
      url = darwinPatchUrl "round-undecorated-frame.patch";
      hash = lib.fakeHash;
    })
    # fix-window-role is only needed for Emacs 30; fixed upstream for 31+
    ++ lib.optional (isDarwin && withFixWindowRolePatch && patchBranch == "30") (fetchpatch {
      name = "fix-window-role.patch";
      url = darwinPatchUrl "fix-window-role.patch";
      hash = lib.fakeHash;
    });

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
  # Skip overrideAttrs for plain mainline builds to preserve binary cache hits.
  needsOverride = isGitVariant || darwinPatches != [ ] || (variant == "macport" && rev != null);

in
if !needsOverride then
  overridden
else
  overridden.overrideAttrs (
    old:
    # Source override for git-based variants
    lib.optionalAttrs isGitVariant {
      src = fetchGitSrc variant;
    }
    # Source override for macport with custom rev
    // macportSrcOverride
    // {
      patches = (old.patches or [ ]) ++ darwinPatches;

      configureFlags =
        (old.configureFlags or [ ])
        # emacs-overlay: fix segfaults on aarch64-linux for git builds
        ++ lib.optionals isAarch64Linux [
          "--enable-check-lisp-object-type"
        ]
        # IGC: enable MPS (Memory Pool System) garbage collector
        ++ lib.optionals (variant == "igc") [
          "--with-mps=yes"
        ];

      buildInputs = (old.buildInputs or [ ]) ++ lib.optionals (variant == "igc") [ pkgs.mps ];

      # Embed git revision so emacs-repository-get-version works
      # without a .git directory in the build tree
      postPatch =
        (old.postPatch or "")
        + lib.optionalString isGitVariant ''
          substituteInPlace lisp/loadup.el \
            --replace-warn '(emacs-repository-get-version)' '"${gitMeta.${variant}.srcRev}"' \
            --replace-warn '(emacs-repository-get-branch)' '"${gitMeta.${variant}.branch}"'
        '';
    }
  )

# emacs.nix: bare GNU Emacs with its build options exposed. For the full
# distribution (packages + tree-sitter grammars), use default.nix.
#
# THE BUILD MATRIX: two builds on {x86_64, aarch64} Linux:
#   * Linux GUI:       pgtk (GTK / Wayland), the only Linux GUI. X11,
#                      Lucid, GTK3-x11, Motif and Athena are asserted away.
#   * Linux terminal:  noGui.
#
# Called standalone (`nix-build emacs.nix`), the default `pkgs` applies
# emacs-overlay pinned from flake.lock.
#
# Usage:
#   nix-build emacs.nix                                            # unstable variant (Emacs 31.1);
#                                                                  #   pgtk GUI
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
  # (a custom `rev`). On a cached
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

  # GUI toolkit: pgtk. GTK3 the library is still a
  # pgtk build input (make-emacs.nix defaults withGTK3 to
  # `withPgtk && !noGui`, left to the base).
  noGui ? false, # terminal only (--without-x --without-ns)
  withPgtk ? (pkgs.stdenv.hostPlatform.isLinux && !noGui),
  # --with-pgtk (pure GTK / Wayland; the Linux GUI)
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
}:

# The build matrix: the only GUI is pgtk on Linux.
assert pkgs.stdenv.hostPlatform.isLinux -> (noGui || withPgtk);
assert builtins.elem variant [
  "mainline"
  "git"
  "unstable"
  "igc"
];

let
  inherit (pkgs) lib fetchgit;

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
          "emacs.nix: variant \"${variant}\" needs nix-community/emacs-overlay "
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
  # and `cpuTune`.
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

  # overrideAttrs only for a custom rev or cpuTune, so every
  # default-rev build stays a pure override (cache hit).
  hasCustomGitSrc = isGitVariant && rev != null;
  needsOverride = hasCustomGitSrc || cpuTune != null;

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

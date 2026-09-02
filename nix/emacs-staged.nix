# nix/emacs-staged.nix — EXPERIMENTAL two-stage from-source Emacs build:
# the C half (temacs) and the Lisp half (byte-compile + dump + install)
# as separate derivations.
#
# Why: a stock Emacs derivation is monolithic, so iterating on a custom
# rev or local patches pays the full C compile + full Lisp compile every
# time.  This file splits the build at the temacs boundary so that
#
#   • a Lisp-side patch (`lispPatches`) invalidates ONLY stageB, and
#   • stageA (the whole C compile) can be pushed to / served from a
#     binary cache once per (src, cPatches, flags) tuple.
#
# What it deliberately is NOT: a replacement for the cache-parity builds
# in emacs.nix / nix/mk-overlay.nix.  Those are byte-identical to the
# emacs-overlay/nixpkgs prebuilts and always preferable when the default
# rev is what you want.  This staged build exists for the from-source
# cases (custom rev, local patches, the permanently-uncached Darwin GUI)
# and is wired only as a legacyPackage + `just build-staged` — never a
# check, never CI.
#
# Mechanics worth knowing before editing:
#
#   • stageA's source is the Emacs source tree MINUS lisp/**/*.el, so
#     lisp-only source diffs don't reach it.  Three exceptions stay in
#     (verified against the 30.2 tree's src/Makefile.in): lisp/loadup.el
#     (src/lisp.mk is *generated from* it, line 579) and
#     lisp/international/ + admin/charsets (temacs's rule depends on
#     $(charsets) $(charscript) ${emoji-zwj}, line 738).
#   • With `contentAddressed = true` the filtered source is also
#     content-addressed, which upgrades "lisp edits don't invalidate
#     stageA" from same-checkout convenience to cutoff across *revision
#     bumps* whose diff stays outside the C half.  Off by default: needs
#     the ca-derivations experimental feature.
#   • Both stages build in /build/jotain-staged so the absolute paths
#     configure bakes into config.status/Makefiles stay valid across the
#     stage boundary.  The handoff is a zstd tar because the Nix store
#     flattens mtimes — inside the tar the make-relevant timestamps
#     survive.
#   • stageB overlays the full source's lisp/ with cp -n and FRESH
#     mtimes: the overlaid .el must be newer than the tarred build tree
#     so make (re)compiles them; -n keeps the files stageA already had
#     (loadup.el, international/) at their tar timestamps.
#   • configure ran in stageA with a placeholder prefix; stageB passes
#     prefix=$out on every make invocation and regenerates epaths.h via
#     the `epaths-force` target (Makefile.in:464).  The fresh
#     epaths.h deliberately triggers recompilation of its includers and
#     a temacs relink — do NOT suppress that with touch games, it is
#     what makes the installed Emacs find its own lisp without
#     EMACSLOADPATH (the smoke test at the end asserts exactly this).
#   • Default configure flags build a terminal-only, non-native-comp
#     Emacs: the point of the experiment is the staging mechanics, and
#     native-comp would AOT-compile the whole lisp tree in stageB.
{
  pkgs,
  lib ? pkgs.lib,
  src ? pkgs.emacs.src,
  version ? pkgs.emacs.version,
  cPatches ? [ ],
  lispPatches ? [ ],
  contentAddressed ? false,
  configureFlags ? [
    "--without-x"
    "--without-ns"
    "--without-native-compilation"
    "--with-gnutls=ifavailable"
    "--with-tree-sitter=ifavailable"
    "--without-xml2"
  ],
}:
let
  caAttrs = lib.optionalAttrs contentAddressed {
    __contentAddressed = true;
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
  };

  buildDirName = "jotain-staged";

  # The C-only source: everything except lisp/**/*.el, minus the
  # exceptions the temacs build genuinely reads (see header).
  srcCOnly = pkgs.runCommand "emacs-src-c-only-${version}" caAttrs ''
    mkdir $out
    cp -r ${src}/. $out/
    chmod -R u+w $out
    find $out/lisp -name '*.el' \
      ! -path '*/international/*' \
      ! -name 'loadup.el' \
      -delete
  '';

  commonNativeBuildInputs = with pkgs; [
    pkg-config
    texinfo
    zstd
  ];
  commonBuildInputs = with pkgs; [
    ncurses
    zlib
  ];

  stageA = pkgs.stdenv.mkDerivation (
    {
      pname = "emacs-staged-a";
      inherit version;
      src = srcCOnly;
      patches = cPatches;

      # autoreconfHook: nixpkgs' emacs.src is a git checkout, not a
      # release tarball — there is no pregenerated ./configure (same
      # reason make-emacs.nix carries the hook).
      nativeBuildInputs = commonNativeBuildInputs ++ [ pkgs.autoreconfHook ];
      buildInputs = commonBuildInputs;

      # Fixed build dir so configure's baked absolute paths survive into
      # stageB (see header).
      unpackPhase = ''
        mkdir -p /build/${buildDirName}
        cp -r $src/. /build/${buildDirName}/
        chmod -R u+w /build/${buildDirName}
        cd /build/${buildDirName}
        sourceRoot=$PWD
      '';

      inherit configureFlags;

      buildPhase = ''
        runHook preBuild
        make -C lib -j$NIX_BUILD_CORES
        make -C src -j$NIX_BUILD_CORES temacs
        runHook postBuild
      '';

      # Hand the whole build tree (objects, config.status, temacs) to
      # stageB with mtimes intact.
      installPhase = ''
        runHook preInstall
        mkdir -p $out
        tar -C /build/${buildDirName} -cf - . | zstd -T$NIX_BUILD_CORES -o $out/build-tree.tar.zst
        runHook postInstall
      '';

      dontFixup = true;
    }
    // caAttrs
  );

  emacs = pkgs.stdenv.mkDerivation (
    {
      pname = "emacs-staged";
      inherit version;

      nativeBuildInputs = commonNativeBuildInputs;
      buildInputs = commonBuildInputs;

      unpackPhase = ''
        mkdir -p /build/${buildDirName}
        zstd -d < ${stageA}/build-tree.tar.zst | tar -C /build/${buildDirName} -xf -
        cd /build/${buildDirName}
        sourceRoot=$PWD

        # Overlay the Lisp half from the full source. -n keeps stageA's
        # files (loadup.el, international/) at their tar mtimes; the
        # newly-added .el get fresh mtimes so make compiles them.
        chmod -R u+w lisp
        cp -rn --no-preserve=mode,timestamps ${src}/lisp/. lisp/
      '';

      patches = lispPatches;

      # The whole point of the staging: configure already ran in stageA
      # and its Makefiles/config.status arrived via the tar.  Without
      # this, stdenv's default configurePhase would re-run ./configure —
      # with the wrong (empty) flags.
      dontConfigure = true;

      buildPhase = ''
        runHook preBuild
        make epaths-force prefix=$out
        make -j$NIX_BUILD_CORES prefix=$out
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        make install prefix=$out
        runHook postInstall
      '';

      # The point of the epaths dance: an installed staged Emacs must
      # find its own lisp with NO environment help.  (locate-library
      # would fall back to EMACSLOADPATH if we exported one — we don't.)
      doInstallCheck = true;
      installCheckPhase = ''
        unset EMACSLOADPATH
        version_out=$($out/bin/emacs --batch --eval '(princ emacs-version)')
        echo "staged emacs reports version: $version_out"
        subr_path=$($out/bin/emacs --batch --eval '(princ (locate-library "subr"))')
        echo "subr resolves to: $subr_path"
        case $subr_path in
          $out/*) ;;
          *) echo "FAIL: staged emacs does not resolve its lisp under \$out"; exit 1 ;;
        esac
      '';

      passthru = {
        inherit stageA srcCOnly;
      };
    }
    // caAttrs
  );
in
emacs

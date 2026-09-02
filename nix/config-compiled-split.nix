# nix/config-compiled-split.nix — Per-file byte-compile (and optionally
# AOT native-compile) of the Jotain Elisp config.
#
# The per-file twin of nix/config-compiled.nix: same call signature, same
# output layout, same compile semantics — but one derivation per source
# file, merged with symlinkJoin.  Editing one module rebuilds that
# module's derivation alone (plus init.el's, see below) instead of
# re-compiling all ~33 files.  The monolith stays the canonical artifact
# (checks.elisp-compile, module.nix); this split is the incremental
# development variant and the groundwork for finer-grained caching.
#
# Design constraints, established the hard way:
#
#   • The module graph is a *star*: no lisp/init-*.el requires another
#     (verified — the one deliberate exception is init-devenv.el, glue
#     for the in-repo lisp/devenv.el library, wired via `compileDeps`
#     below).  Cross-file symbol references are declared with
#     `declare-function`/`defvar` stubs in the sources themselves, NOT
#     by adding inter-module requires here: a require would drag the
#     required file into this fileset and erode the per-file
#     incrementality this file exists to provide.  Keep it that way.
#   • init.el executes its thirty top-level `(require 'init-…)` forms
#     *during byte-compilation*, so its derivation must see every module
#     source.  Its input is therefore the whole lisp/ tree and it
#     rebuilds on any module edit — acceptable, the file is tiny.
#   • Each derivation's OUTPUT is pruned to its own file(s).  Without
#     the pruning, the init.el derivation would also carry uncompiled
#     copies of every module and symlinkJoin would pick a provider
#     arbitrarily — separating a module's .el from its .elc and breaking
#     the eln realpath contract described below.
#   • The compile preamble mirrors nix/config-compiled.nix exactly,
#     including ORDER: pcre2el is required *before*
#     `byte-compile-error-on-warn` is set (loading it after the flag is
#     the original failure mode — see the monolith's header comment and
#     journal/2026-04-16.md).
#   • Compilation happens *inside $out* (the target file is copied there
#     first, dependencies stay on `-L`-provided store paths).  This is
#     what makes the optional native pass sound: `comp-el-to-eln-rel-filename`
#     hashes the realpath of the source file into the .eln name, so the
#     .eln must be compiled from the same absolute path that consumers
#     will resolve through the symlinkJoin later (Bug#44701 semantics —
#     see nix/config-compiled.nix).
#
# `contentAddressed = true` additionally marks every per-file derivation
# content-addressed (`__contentAddressed`), giving early cutoff:
# a recompile that produces identical output does not invalidate
# downstream consumers.  It requires the `ca-derivations` experimental
# feature on the evaluating/building Nix and is therefore OFF by default
# and never enabled in CI.  Without it the split still gives per-file
# *rebuild* granularity, just not cutoff — an unchanged-output rebuild
# still ripples into the join.
{
  pkgs,
  lib ? pkgs.lib,
  # An emacsWithPackages-style distribution; pass `.core` where it
  # exists (see nix/config-compiled.nix's header note).
  emacs,
  src ? ../.,
  nativeCompile ? false,
  contentAddressed ? false,
}:
let
  inherit (lib) fileset;

  caAttrs = lib.optionalAttrs contentAddressed {
    __contentAddressed = true;
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
  };

  # Compile-time source dependencies beyond the file itself, by basename.
  # Only for files whose byte-compilation must LOAD another in-repo file
  # (use-package's compile-time require).  Everything else is covered by
  # declare-function/defvar stubs in the sources.
  compileDeps = {
    "init-devenv.el" = [ "devenv.el" ];
  };

  lispNames = lib.naturalSort (
    builtins.attrNames (
      lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".el" name) (
        builtins.readDir (src + "/lisp")
      )
    )
  );

  # Sources the compiler may need to *load* while compiling any file:
  # the full lisp/ tree.  Kept as one store path passed via -L; it is an
  # input only of the derivations whose fileset includes it (init.el).
  allLispSrc = fileset.toSource {
    root = src;
    fileset = fileset.fileFilter (f: lib.hasSuffix ".el" f.name) (src + "/lisp");
  };

  # One compile derivation.
  #   relPath  — path of the target relative to the config root
  #   srcFs    — fileset for this derivation's input
  #   loadDirs — extra `-L` dirs for compile-time requires (store paths
  #              that are NOT part of this derivation's input closure
  #              only when listed in srcFs; init.el passes allLispSrc)
  compileFile =
    {
      relPath,
      srcFs,
      loadDirs ? [ ],
    }:
    let
      fsSrc = fileset.toSource {
        root = src;
        fileset = srcFs;
      };
      loadFlags = lib.concatMapStringsSep " " (d: "-L ${d}/lisp") loadDirs;
    in
    pkgs.runCommand "jotain-elc-${builtins.replaceStrings [ "/" ] [ "-" ] relPath}"
      (
        {
          nativeBuildInputs = [ emacs ];
        }
        // caAttrs
      )
      ''
        mkdir -p $out/$(dirname ${relPath})
        cp ${fsSrc}/${relPath} $out/${relPath}
        chmod -R u+w $out
        cd $out

        # Same preamble and order as nix/config-compiled.nix (see header).
        emacs --batch \
          -L ${fsSrc}/lisp ${loadFlags} \
          --eval "(require 'pcre2el)" \
          --eval "(setq byte-compile-error-on-warn t)" \
          -f batch-byte-compile ${relPath}

        ${lib.optionalString nativeCompile ''
          mkdir -p $out/share/emacs/native-lisp
          emacs --batch \
            -L ${fsSrc}/lisp ${loadFlags} \
            --eval "(setq native-comp-speed 2 native-comp-async-jobs-number 0)" \
            --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
            -f batch-native-compile ${relPath}
        ''}
      '';

  components = {
    "early-init.el" = compileFile {
      relPath = "early-init.el";
      srcFs = src + "/early-init.el";
    };
    # init.el's compile executes its requires: whole lisp/ tree on -L.
    "init.el" = compileFile {
      relPath = "init.el";
      srcFs = src + "/init.el";
      loadDirs = [ allLispSrc ];
    };
  }
  // lib.listToAttrs (
    map (name: {
      inherit name;
      value = compileFile {
        relPath = "lisp/${name}";
        srcFs = fileset.unions (
          [ (src + "/lisp/${name}") ] ++ map (d: src + "/lisp/${d}") (compileDeps.${name} or [ ])
        );
      };
    }) lispNames
  );
in
pkgs.symlinkJoin {
  name = "jotain-config-compiled-split";
  paths = lib.attrValues components;
  passthru = {
    inherit nativeCompile contentAddressed components;
  };
}

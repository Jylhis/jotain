# nix/config-compiled.nix — Byte-compile (and optionally AOT
# native-compile) the Jotain Elisp config.
#
# One derivation, two consumers:
#   • nix/checks.nix `elisp-compile' — the warnings-as-errors gate.
#   • module.nix     `compiledConfig' — what the HM daemon loads.
#
# These used to be two copies of the same `emacs --batch' invocation
# (nix/checks.nix and module.nix), so CI proved a byte-compile that the
# deploy then redid from a different source root.  Now the artifact CI
# builds — and that deploy.yml pushes to cachix — is the artifact the
# daemon installs, so `home-manager switch' substitutes it instead of
# running Emacs locally.
#
# Two deliberate narrowings versus the old checks.nix version:
#
#   • The source is a `lib.fileset' of just the config's .el files
#     rather than the whole flake tree, so a README/website/journal edit
#     no longer invalidates the byte-compile.
#   • Callers pass `emacs = <distribution>.core' — the *inner*
#     emacsWithPackages result, not the outer jotain-emacs-full wrapper.
#     The wrapper adds a runtime PATH, INFOPATH (→ jotainInfo →
#     packages-doc → options-doc) and ASPELL_CONF; none is read by a
#     batch byte-compile, and depending on it meant every `;;; @doc'
#     block and every docs/*.mdx page invalidated this derivation.
{
  pkgs,
  lib ? pkgs.lib,
  # An emacsWithPackages-style distribution.  Pass `.core' where it
  # exists (see the header note).
  emacs,
  src ? ../.,
  # AOT native-compile into $out/share/emacs/native-lisp so the daemon
  # loads store .eln for init.el and lisp/ instead of JIT-compiling
  # into var/eln-cache after every deploy.  The
  # store-.eln-through-symlinks mechanism is verified sound at the
  # source level: `comp-el-to-eln-rel-filename` (src/comp.c) calls
  # realpath() on the source before hashing it into the .eln name —
  # "Resolve possible symlinks in FILENAME, so that path_hash below
  # always compares equal. (Bug#44701)" — so module.nix's
  # xdg.configFile symlinks resolve to the exact store path this
  # derivation compiled against.  Two scope limits: early-init.el's
  # .eln is structurally unreachable (its eln lookup runs before
  # early-init.el itself extends native-comp-eln-load-path), and the
  # verification was read on the Emacs 30.2 source tree while this
  # repo ships 31 — the mechanism predates both.  Off by default only
  # for the cost: an extra native-comp pass whenever the config
  # rebuilds, and ~50-150 MB of .eln in the closure.
  nativeCompile ? false,
}:
let
  inherit (lib) fileset;

  # Exactly what the compile command below reads.  `lib.fileset' needs a
  # real path, not the string-like flake source — the same constraint
  # flake.nix documents on `packages.site'.  Inside a flake, ../. is the
  # store copy of the tree, so .git and .gitignore'd files (stray *.elc,
  # result symlinks) are already excluded; the .el filter keeps a
  # non-flake `import' honest too.
  configSrc = fileset.toSource {
    root = src;
    fileset = fileset.unions [
      (src + "/early-init.el")
      (src + "/init.el")
      (fileset.fileFilter (f: lib.hasSuffix ".el" f.name) (src + "/lisp"))
    ];
  };
in
pkgs.runCommand "jotain-config-compiled"
  {
    src = configSrc;
    nativeBuildInputs = [ emacs ];
    passthru = { inherit nativeCompile; };
  }
  ''
    mkdir -p $out
    cp -r $src/. $out/
    chmod -R u+w $out
    cd $out

    # The .el sources are kept beside the .elc so `find-function' and
    # native compilation can still read them.
    #
    # The pcre2el require is load-bearing: magit-todos propagates
    # pcre2el, whose defadvice byte-compiles its advice body and fails
    # under error-on-warn (see journal/2026-04-16.md).
    emacs --batch \
      -L lisp \
      --eval "(require 'pcre2el)" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile early-init.el init.el lisp/devenv.el lisp/init-*.el

    ${lib.optionalString nativeCompile ''
      # Modelled on nixpkgs build-support/emacs/generic.nix' postInstall:
      # `comp-el-to-eln-filename' writes to (car native-comp-eln-load-path)
      # and `add-to-list' prepends, so the .eln land under
      # $out/share/emacs/native-lisp/<comp-native-version-dir>/.
      #
      # native-comp-speed is pinned to 2 to match early-init.el, because
      # the speed setting is NOT part of the .eln hash — an eln compiled
      # at a different speed would silently shadow this one.
      mkdir -p $out/share/emacs/native-lisp
      emacs --batch \
        -L lisp \
        --eval "(setq native-comp-speed 2 native-comp-async-jobs-number 0)" \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
        -f batch-native-compile early-init.el init.el lisp/devenv.el lisp/init-*.el
    ''}
  ''

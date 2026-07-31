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
  # loads store .eln instead of JIT-compiling into var/eln-cache after
  # every deploy.  Off by default: it only pays off if Emacs resolves
  # the source path through symlinks before hashing it into the .eln
  # filename, which is deployment-specific.  Verify before enabling:
  #
  #   mkdir -p /tmp/elntest/real && cp lisp/init-core.el /tmp/elntest/real/
  #   ln -s /tmp/elntest/real /tmp/elntest/link
  #   nix run .#emacs -- --batch --eval '
  #    (progn (require (quote comp))
  #      (princ (format "real=%s\nlink=%s\n"
  #        (comp-el-to-eln-rel-filename "/tmp/elntest/real/init-core.el")
  #        (comp-el-to-eln-rel-filename "/tmp/elntest/link/init-core.el"))))'
  #
  # Identical strings → the store .eln is reachable through the
  # xdg.configFile symlinks module.nix installs.  Different → leave this
  # off; early-init.el's var/eln-cache JIT path is then the only thing
  # that can work.
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

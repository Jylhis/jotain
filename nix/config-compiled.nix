# Byte-compile (and optionally native-compile) the Jotain config. Used by the
# `elisp-compile` check and module.nix's `compiledConfig`.
{
  pkgs,
  lib ? pkgs.lib,
  # Pass the distribution's `.core`: the outer wrapper's INFOPATH would
  # invalidate this on docs edits.
  emacs,
  src ? ../.,
  # The source is realpath()ed before hashing into the .eln name (Bug#44701),
  # so module.nix's xdg.configFile symlinks resolve to the store path
  # compiled here. early-init.el's .eln is unreachable: its lookup runs
  # before early-init.el extends native-comp-eln-load-path.
  nativeCompile ? false,
}:
let
  inherit (lib) fileset;

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

    # magit-todos propagates pcre2el, whose defadvice byte-compiles its
    # advice body and fails under error-on-warn unless it is loaded first.
    emacs --batch \
      -L lisp \
      --eval "(require 'pcre2el)" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile early-init.el init.el lisp/devenv.el lisp/repo-review.el lisp/init-*.el

    ${lib.optionalString nativeCompile ''
      # Speed is not part of the .eln hash, so keep it at the default of 2.
      mkdir -p $out/share/emacs/native-lisp
      emacs --batch \
        -L lisp \
        --eval "(setq native-comp-speed 2 native-comp-async-jobs-number 0)" \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
        -f batch-native-compile early-init.el init.el lisp/devenv.el lisp/repo-review.el lisp/init-*.el
    ''}
  ''

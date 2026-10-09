# nix/config-compiled.nix — Byte-compile (and optionally AOT
# native-compile) the Jotain Elisp config.
#
# One derivation, used by nix/checks.nix `elisp-compile' (the
# warnings-as-errors gate) and module.nix `compiledConfig' (what the HM
# daemon loads). deploy.yml pushes it to cachix, so `home-manager switch'
# substitutes CI's artifact.
#
# Two narrowings keep it from rebuilding on unrelated edits:
#   • the source is a `lib.fileset' of just the config's .el files;
#   • callers pass `emacs = <distribution>.core', the inner
#     emacsWithPackages result. The outer wrapper adds PATH, INFOPATH
#     (→ jotainInfo → every `;;; @doc' block and docs/*.mdx page) and
#     ASPELL_CONF, none of which a batch byte-compile reads.
{
  pkgs,
  lib ? pkgs.lib,
  # An emacsWithPackages-style distribution; pass `.core' where it exists.
  emacs,
  src ? ../.,
  # AOT native-compile into $out/share/emacs/native-lisp, so the daemon
  # loads store .eln for init.el and lisp/ instead of JIT-compiling after
  # every deploy. `comp-el-to-eln-rel-filename' (src/comp.c) realpath()s
  # the source before hashing it into the .eln name (Bug#44701), so
  # module.nix's xdg.configFile symlinks resolve to the store path
  # compiled here. early-init.el's .eln is unreachable: its lookup runs
  # before early-init.el extends native-comp-eln-load-path. Off by default
  # for the cost: an extra native-comp pass and ~50-150 MB of .eln.
  nativeCompile ? false,
}:
let
  inherit (lib) fileset;

  # Exactly what the compile command reads. `src' must be a real path
  # (lib.fileset rejects the flake source string); the .el filter also
  # keeps a non-flake import free of stray files.
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

    # The .el sources stay beside the .elc for `find-function' and
    # native compilation.
    #
    # The pcre2el require is load-bearing: magit-todos propagates
    # pcre2el, whose defadvice byte-compiles its advice body and fails
    # under error-on-warn.
    emacs --batch \
      -L lisp \
      --eval "(require 'pcre2el)" \
      --eval "(setq byte-compile-error-on-warn t)" \
      -f batch-byte-compile early-init.el init.el lisp/devenv.el lisp/init-*.el

    ${lib.optionalString nativeCompile ''
      # Modelled on nixpkgs build-support/emacs/generic.nix' postInstall:
      # .eln files go to (car native-comp-eln-load-path), which
      # `add-to-list' makes $out/share/emacs/native-lisp/.
      #
      # native-comp-speed matches early-init.el's 2: speed is not part of
      # the .eln hash, so an .eln at another speed would silently shadow it.
      mkdir -p $out/share/emacs/native-lisp
      emacs --batch \
        -L lisp \
        --eval "(setq native-comp-speed 2 native-comp-async-jobs-number 0)" \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
        -f batch-native-compile early-init.el init.el lisp/devenv.el lisp/init-*.el
    ''}
  ''

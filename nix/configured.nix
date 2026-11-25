# The distribution plus the Jotain config: `bin/emacs` starts with a
# writable init directory whose entries link to the byte-compiled config.
{
  pkgs,
  # jotainEmacsPackages (the wrapped distribution).
  package,
  name ? "jotain-emacs-configured",
  src ? ../.,
}:
let

  config = import ./config-compiled.nix {
    inherit pkgs src;
    emacs = package.core;
  };

  # The config writes var/, elpa/ and the eln cache under
  # user-emacs-directory, so that directory cannot be a store path.
  launcher = pkgs.writeShellApplication {
    name = "emacs";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      for arg in "$@"; do
        case "$arg" in
          --init-directory | --init-directory=*)
            exec ${package}/bin/emacs "$@"
            ;;
        esac
      done

      dir="''${JOTAIN_INIT_DIRECTORY:-''${XDG_STATE_HOME:-$HOME/.local/state}/jotain}"
      mkdir -p "$dir"
      for f in early-init.el early-init.elc init.el init.elc lisp; do
        ln -sfn ${config}/"$f" "$dir/$f"
      done
      ln -sfn ${src + "/templates"} "$dir/templates"

      exec ${package}/bin/emacs --init-directory="$dir" "$@"
    '';
  };
in
pkgs.runCommand name
  {
    nativeBuildInputs = [ (pkgs.lndir or pkgs.xorg.lndir) ];
    meta = (package.meta or { }) // {
      mainProgram = "emacs";
    };
    passthru = (package.passthru or { }) // {
      inherit (package) core package;
      inherit config;
    };
  }
  ''
    mkdir -p $out
    lndir -silent ${package} $out
    for prog in $out/bin/emacs $out/bin/emacs-[0-9]*; do
      [ -e "$prog" ] || continue
      rm "$prog"
      ln -s ${launcher}/bin/emacs "$prog"
    done
  ''

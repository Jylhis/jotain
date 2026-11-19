{
  pkgs,
  package,
  runtimeDeps,
  # [ ] keeps the caller's ASPELL_CONF (or none).
  spellDictionaries ? [ ],
  # --set-default (the user may override) instead of --set.
  aspellDefault ? false,
  infoDirs ? [ ],
  name ? "${package.name or "jotain-emacs"}-with-runtime-deps",
  passthru ? package.passthru or { },
}:
let
  inherit (pkgs) lib;
  aspellConf = import ./spell-conf.nix {
    inherit pkgs;
    dictionaries = spellDictionaries;
  };
  aspellFlag = if aspellDefault then "--set-default" else "--set";
in
pkgs.runCommand name
  {
    nativeBuildInputs = [
      # Top-level `lndir` only exists on recent nixpkgs.
      (pkgs.lndir or pkgs.xorg.lndir)
      pkgs.makeBinaryWrapper
    ];
    meta = (package.meta or { }) // {
      mainProgram = "emacs";
    };
    inherit passthru;
  }
  ''
    mkdir -p $out
    lndir -silent ${package} $out
    for prog in $out/bin/*; do
      [ -L "$prog" ] || continue
      orig=$(readlink -f "$prog")
      rm "$prog"
      makeBinaryWrapper "$orig" "$prog" \
        --suffix PATH : "${lib.makeBinPath runtimeDeps}" ${
          lib.concatMapStringsSep " " (d: "--suffix INFOPATH : ${lib.escapeShellArg d}") infoDirs
        } ${
          lib.optionalString (aspellConf != null) "${aspellFlag} ASPELL_CONF ${lib.escapeShellArg aspellConf}"
        }
    done
  ''

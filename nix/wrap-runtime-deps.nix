# nix/wrap-runtime-deps.nix: re-wrap an Emacs configuration distribution with the
# runtime binaries (nix/runtime-deps.nix) appended to PATH. Shared by
# module-system.nix only.
#
# Appending keeps the host userland first. The wrapper PATH survives every
# launch context, unlike environment.systemPackages.
{
  pkgs,
  package,
  runtimeDeps,
  # services.emacsConfig.spell.dictionaries; [ ] keeps the bundled dictionaries.
  spellDictionaries ? [ ],
}:
let
  aspellConf = import ./spell-conf.nix {
    inherit pkgs;
    dictionaries = spellDictionaries;
  };
in
pkgs.runCommand "${package.name or "emacs-config"}-with-runtime-deps"
  {
    nativeBuildInputs = [
      # Top-level `lndir` only exists on recent nixpkgs; on older
      # releases (24.05+) it lives under the xorg package set.
      (pkgs.lndir or pkgs.xorg.lndir)
      pkgs.makeBinaryWrapper
    ];
    meta = (package.meta or { }) // {
      mainProgram = "emacs";
    };
    passthru = package.passthru or { };
  }
  ''
    mkdir -p $out
    lndir -silent ${package} $out
    for prog in $out/bin/*; do
      [ -L "$prog" ] || continue
      orig=$(readlink -f "$prog")
      rm "$prog"
      makeBinaryWrapper "$orig" "$prog" \
        --suffix PATH : "${pkgs.lib.makeBinPath runtimeDeps}" ${
          pkgs.lib.optionalString (
            aspellConf != null
          ) "--set ASPELL_CONF ${pkgs.lib.escapeShellArg aspellConf}"
        }
    done
  ''

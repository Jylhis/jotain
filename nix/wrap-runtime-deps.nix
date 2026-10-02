# nix/wrap-runtime-deps.nix — Re-wrap a Jotain Emacs distribution so the
# runtime binaries (nix/runtime-deps.nix) ride the Emacs PATH instead of
# the global environment. Shared by module-system.nix and
# module-nix-on-droid.nix.
#
# Appending keeps the host userland first (and GNU coreutils out of the
# way on darwin), and — unlike environment.systemPackages, which a
# Dock/launchd-launched GUI Emacs never sees on darwin — the wrapper PATH
# survives every launch context.
{
  pkgs,
  package,
  runtimeDeps,
}:
pkgs.runCommand "${package.name or "jotain-emacs"}-with-runtime-deps"
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
        --suffix PATH : "${pkgs.lib.makeBinPath runtimeDeps}"
    done
  ''

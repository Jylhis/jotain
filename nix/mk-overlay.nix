{
  # Emacs source variant for jotainEmacs / jotainEmacsNoGui (see
  # emacs.nix). Defaults to "unstable": emacs-overlay's emacs-unstable,
  # the newest Emacs release or pretest tag (currently 31.1), cached on
  # nix-community.cachix.org.
  variant ? "unstable",
}:
final: _prev:
let
  usePackage = import ./use-package.nix { inherit (final) lib; };
  extraPackages = import ./extra-packages.nix { pkgs = final; };

  # Packages injected outside the lisp/ use-package scan (see that file).
  nixProvidedPackages = import ./nix-provided-packages.nix;

  # Runtime binaries the config shells out to, on every distribution
  # wrapper's PATH so a bare `just run-built' is self-contained.
  # `--suffix' keeps ambient/project tools first, so a devenv-provided
  # server still wins.
  runtimeDeps = import ./runtime-deps.nix {
    pkgs = final;
    pkgsWithOverlay = final;
  };

  # Bundled dictionaries so jinx (lisp/init-writing.el) works without a
  # populated profile; otherwise a bare `./result/bin/emacs' reports
  # `No dictionaries available'.
  #
  # `aspellWithDicts' puts libaspell's data files and the dictionaries in
  # one `lib/aspell', and ASPELL_CONF points both dict-dir and data-dir at
  # it. NIX_PROFILES alone is not enough: nixpkgs' libaspell patch only
  # uses it for dictionary enumeration, so `enchant_broker_request_dict'
  # (what jinx calls) still fails, as the master word list resolves under
  # the default data-dir. en_GB is jinx's default; fi/de/fr are a C-M-$
  # switch away.
  spellEnv = final.aspellWithDicts (
    d: with d; [
      en
      fi
      de
      fr
    ]
  );
  spellConf = "dict-dir ${spellEnv}/lib/aspell; data-dir ${spellEnv}/lib/aspell";

  mkJotainEmacsPackages =
    {
      name,
      package,
    }:
    let
      core = usePackage.emacsWithPackagesFromUsePackage {
        config = ../lisp;
        inherit package;
        inherit (final) emacsPackagesFor;
        override = extraPackages;
        # Fail the build, listing every miss, if a declared package is
        # absent from the package set (e.g. under an older consumer
        # nixpkgs) instead of silently shipping a broken editor.
        strict = true;
        # Unguarded `epkgs.<name>' lookups on purpose: a missing attr must
        # fail loudly (see nix-provided-packages.nix). Every nixpkgs in
        # [24.05, unstable] ships these, so the 24.05+ override path holds.
        extraEmacsPackages =
          epkgs:
          map (n: epkgs.${n}) nixProvidedPackages
          ++ [
            # Full grammar set: a linkFarm over cached upstream
            # derivations, so it costs closure size (~200 MB over a
            # curated subset, measured 2026-08-01), never build time, and
            # keeps the `jotain-emacs-full' hash cache-stable.
            epkgs.treesit-grammars.with-all-grammars
          ];
      };
    in
    final.runCommand name
      {
        nativeBuildInputs = [
          # Top-level `lndir` only exists on recent nixpkgs; on older
          # releases (24.05+) it lives under the xorg package set.
          (final.lndir or final.xorg.lndir)
          final.makeBinaryWrapper
        ];
        meta = (core.meta or { }) // {
          mainProgram = "emacs";
        };
        passthru = (core.passthru or { }) // {
          inherit core package;
          info = final.jotainInfo;
        };
      }
      ''
        mkdir -p $out
        lndir -silent ${core} $out

        # Re-wrap anything under bin/ that may launch Emacs, so the info
        # path propagates through `emacs', `emacsclient', the `.app'
        # bundle (macOS), and any helper tools that shell out to emacs.
        for prog in $out/bin/*; do
          [ -L "$prog" ] || continue
          orig=$(readlink -f "$prog")
          rm "$prog"
          makeBinaryWrapper "$orig" "$prog" \
            --suffix PATH : "${final.lib.makeBinPath runtimeDeps}" \
            --suffix INFOPATH : "${final.jotainInfo}/share/info" \
            --set-default ASPELL_CONF "${spellConf}"
        done
      '';
in
{
  # Linux GUI is pgtk: it honors each backend's scale (Wayland fractional
  # scale, Xft.dpi/GDK_SCALE on X11), so a fixed point size tracks the
  # system. emacs.nix selects the prebuilt `*-pgtk` sibling, so this stays
  # a binary-cache hit.
  #
  # FLAG-TRIM POLICY: unused features (mailutils' movemail, gpm, SELinux)
  # are dropped ONLY on builds already off binary-cache parity: the two
  # noGui builds (cached only on jylhis cachix) and the Darwin GUI
  # (patched, never cached). The Linux pgtk GUI keeps upstream defaults:
  # it is byte-identical to nix-community's emacs-unstable-pgtk, and that
  # cache hit (Emacs plus every dependent ELPA package) is worth more than
  # any closure trim.
  jotainEmacs = import ../emacs.nix (
    {
      pkgs = final;
      inherit variant;
      withPgtk = final.stdenv.hostPlatform.isLinux;
    }
    // final.lib.optionalAttrs final.stdenv.hostPlatform.isDarwin {
      # Off-parity anyway (NS patches). gpm/selinux are already off here.
      withMailutils = false;
    }
  );

  # Terminal-only build (nix-on-droid, headless). Off-parity by
  # construction, so the trim policy applies.
  jotainEmacsNoGui = import ../emacs.nix {
    pkgs = final;
    inherit variant;
    noGui = true;
    withMailutils = false;
    withGpm = false;
    withSelinux = false;
  };

  # Prebuilt ECA server for eca-emacs (lisp/init-ai.el).
  eca = import ./eca-server.nix { pkgs = final; };

  # LikeC4 language server for `likec4-mode' (lisp/init-lang-devops.el).
  likec4Lsp = import ./likec4-lsp.nix { pkgs = final; };

  jotainInfo = import ./info-manual.nix {
    pkgs = final;
    src = ../.;
  };

  # The full distribution: mkJotainEmacsPackages lndirs the inner
  # emacsWithPackages result (`core', whose share/info we cannot mutate)
  # and re-wraps its binaries with the runtime PATH, ASPELL_CONF and
  # ${jotainInfo}/share/info on INFOPATH, so `C-h i d' finds jotain.info.
  #
  # INFOPATH gets the bare directory: makeBinaryWrapper rejects a value
  # that would create an empty PATH-like segment (GHSA-p7v3-pr2c-8584),
  # so the trailing ':' that makes info-initialize append
  # Info-default-directory-list must come from nixpkgs' site-start.el
  # (Emacs bug#81105), which adds it, keeping the built-in manuals visible.
  jotainEmacsPackages = mkJotainEmacsPackages {
    name = "jotain-emacs-full";
    package = final.jotainEmacs;
  };

  # Full distribution on the terminal-only base (nix-on-droid / headless).
  jotainEmacsPackagesNoGui = mkJotainEmacsPackages {
    name = "jotain-emacs-full-nox";
    package = final.jotainEmacsNoGui;
  };
}

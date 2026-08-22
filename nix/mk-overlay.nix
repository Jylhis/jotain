{
  # Emacs source variant for jotainEmacs / jotainEmacsNoGui (see
  # emacs.nix). Defaults to "unstable": emacs-overlay's emacs-unstable,
  # the Emacs 31 release branch (currently the 31.0.90 pretest), cached
  # on nix-community.cachix.org.
  variant ? "unstable",
}:
final: _prev:
let
  usePackage = import ./use-package.nix { inherit (final) lib; };
  extraPackages = import ./extra-packages.nix { pkgs = final; };

  # Emacs packages the config gets from Nix (nix/extra-packages.nix or the
  # base scope) rather than the lisp/ use-package scan — the single source
  # of truth shared with nix/emacs-api-doc.nix. See that file's header.
  nixProvidedPackages = import ./nix-provided-packages.nix;

  # Runtime binaries the Elisp config shells out to (nix/runtime-deps.nix).
  # Bundled onto every distribution wrapper's PATH below with `--suffix', so
  # a bare `just run-built' is self-contained (carries the shipped nixd LSP
  # fallback and friends) — not only the module/daemon installs, which wrap
  # this same list. `--suffix' keeps ambient / project tools ahead of the
  # bundled copies, so a devenv-provided server still wins.
  runtimeDeps = import ./runtime-deps.nix {
    pkgs = final;
    pkgsWithOverlay = final;
  };

  # Spell dictionaries bundled into the distribution so `jinx' works out of
  # the box (lisp/init-writing.el) without an externally-populated profile.
  # jinx links enchant, whose aspell backend delegates to libaspell, but the
  # base distribution ships no dictionary data — so a bare `./result/bin/emacs'
  # (e.g. `just run-built') reports `No dictionaries available for "en_US"'.
  #
  # `aspellWithDicts' builds one directory holding libaspell's own data files
  # *and* the requested dictionaries under a single `lib/aspell'. We point
  # both aspell `dict-dir' and `data-dir' at it via ASPELL_CONF on the wrapper
  # below. NIX_PROFILES alone is NOT enough: nixpkgs' libaspell patch only
  # feeds NIX_PROFILES into dictionary *enumeration*, so `enchant_broker_-
  # list_dicts' sees the language but `enchant_broker_request_dict' (what jinx
  # calls) still fails to build a speller because the master word list resolves
  # under the default data-dir. en_GB is the jinx default (init-writing.el);
  # fi/de/fr are reachable per buffer via C-M-$.
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
        # Fail the build (listing every miss) if a use-package-declared
        # package is absent from the emacs package set, rather than
        # silently dropping it and shipping a broken editor. This is what
        # makes the older nixpkgs snapshot a safe source to build against.
        strict = true;
        # The Nix-provided packages (shared list; see nix-provided-packages.nix).
        # Each is a flat `epkgs.<name>` lookup, unguarded on purpose: e.g.
        # `nix-ts-mode` is declared `:ensure nil` in init-lang-nix.el, so the
        # lisp/ scan skips it — a missing attr here should be a loud failure,
        # not a silently broken `.nix' autoload. Every nixpkgs in
        # [24.05, unstable] ships these, so the 24.05+ override path is fine.
        extraEmacsPackages =
          epkgs:
          map (n: epkgs.${n}) nixProvidedPackages
          ++ [
            # Full grammar set. Every grammar is its own upstream
            # derivation and this is a linkFarm over their store paths, so
            # the set costs closure size (~200 MB over a curated subset,
            # measured 2026-08-01), never build time. The curated-subset
            # variant (`emacs-lite`) was removed with the build-matrix
            # narrowing; the full set keeps the `jotain-emacs-full` hash
            # cache-stable.
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
            --suffix INFOPATH : "${final.jotainInfo}/share/info:" \
            --set-default ASPELL_CONF "${spellConf}"
        done
      '';
in
{
  # Linux GUI default is pgtk (pure GTK): unlike the X11/Lucid build it
  # honors each backend's advertised scale — hyprland/KDE/GNOME Wayland
  # fractional scale, and Xft.dpi/GDK_SCALE when GDK runs on X11 — so a
  # fixed point size tracks the system like other native GTK apps. pgtk
  # selects emacs-overlay's prebuilt `*-pgtk` sibling in emacs.nix, so
  # this stays a binary-cache hit. Darwin keeps its NS default (retina
  # already scales); the noGui build below never gets a GUI toolkit.
  # FLAG-TRIM POLICY: features this config never uses (mailutils'
  # movemail — rmail-only; gpm console mouse; SELinux attrs) are dropped
  # ONLY on builds that are already off binary-cache parity — the two
  # noGui builds (pure overrides, cached solely on jylhis cachix) and
  # the Darwin GUI (patched by default, never cached anywhere). The
  # Linux pgtk GUI keeps upstream defaults untouched: it is
  # byte-identical to nix-community's emacs-unstable-pgtk, and that
  # cache hit — Emacs plus every dependent ELPA package — is worth more
  # than any closure trim.
  jotainEmacs = import ../emacs.nix (
    {
      pkgs = final;
      inherit variant;
      withPgtk = final.stdenv.hostPlatform.isLinux;
    }
    // final.lib.optionalAttrs final.stdenv.hostPlatform.isDarwin {
      # Off-parity anyway (default-on NS patches): trim what's unused.
      # gpm/selinux are Linux-only and already off on Darwin.
      withMailutils = false;
    }
  );

  # Terminal-only (`-nw`) build, used by the nix-on-droid module: Android
  # under proot is headless, so a GUI Emacs would only bloat the closure
  # with unusable X/Wayland libraries. Off-parity by construction (noGui
  # is an override of the non-pgtk base), so the trim policy applies.
  jotainEmacsNoGui = import ../emacs.nix {
    pkgs = final;
    inherit variant;
    noGui = true;
    withMailutils = false;
    withGpm = false;
    withSelinux = false;
  };

  sonarlintLs = final.sonarlint-ls;

  # Prebuilt ECA server binary for the eca-emacs client (lisp/init-ai.el).
  # Surfaced on the overlay so module-system / Home Manager consumers can put
  # it on the wrapper PATH.
  eca = import ./eca-server.nix { pkgs = final; };

  # LikeC4 language server for `likec4-mode' (lisp/init-lang-devops.el).
  # Surfaced on the overlay for the same reason as `eca'; bundled onto every
  # distribution wrapper's PATH via nix/runtime-deps.nix.
  likec4Lsp = import ./likec4-lsp.nix { pkgs = final; };

  jotainInfo = import ./info-manual.nix {
    pkgs = final;
    src = ../.;
  };

  # Wrap the Emacs-with-packages output so jotain.info becomes
  # discoverable from `C-h i d' without touching user Elisp config.
  #
  # emacsWithPackages builds a wrapper whose share/info is a hard
  # symlink to the bare emacs.nix derivation, which we cannot mutate.
  # Instead we produce an outer derivation that (a) lndirs the core
  # wrapper verbatim, and (b) re-wraps the user-facing binaries to
  # append ${jotainInfo}/share/info to $INFOPATH.  The trailing ':'
  # tells Emacs's info-initialize to append Info-default-directory-list
  # so the built-in Emacs manuals stay visible.
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

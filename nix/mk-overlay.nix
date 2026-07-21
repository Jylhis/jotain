{
  jylhisEmacsSrc ? null,
  # Emacs source variant for jotainEmacs / jotainEmacsNoGui (see
  # emacs.nix). Defaults to "unstable": emacs-overlay's emacs-unstable,
  # the Emacs 31 release branch (currently the 31.0.90 pretest), cached
  # on nix-community.cachix.org. "mainline" (nixpkgs' default Emacs 30
  # attr, Hydra-cached) stays available — the flake exposes it as
  # `packages.emacs-mainline`.
  variant ? "unstable",
  # When true, bundle only the tree-sitter grammars this config actually
  # routes (~26) instead of the full set (~275). Smaller closure / much
  # less to build from source. Default false keeps with-all-grammars so
  # the `jotain-emacs-full` derivation hash is unchanged and the binary
  # cache still hits — see flake.nix `emacs-lite` for the opt-in path.
  curatedGrammars ? false,
}:
final: _prev:
let
  usePackage = import ./use-package.nix { inherit (final) lib; };
  extraPackages = import ./extra-packages.nix { pkgs = final; };

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
        # `nix-ts-mode` is referenced directly (not guarded): init-lang-nix.el
        # declares it `:ensure nil`, so if a nixpkgs snapshot ever lacks it we
        # want a loud build failure rather than silently shipping a broken
        # autoload for `.nix' files. Every nixpkgs in [24.05, unstable] ships
        # it, so the 24.05+ override path is unaffected.
        extraEmacsPackages = epkgs: [
          epkgs.claude-code-ide
          epkgs.combobulate
          epkgs.jylhis-emacs-themes
          epkgs.majutsu
          epkgs.nix-ts-mode
          epkgs.tagref
          (
            if curatedGrammars then
              # Only the grammars the Jotain config routes via the
              # `jotain-prog-ts-remaps' table, init-lang-* `:mode' entries,
              # or combobulate (lisp/init-prog.el, init-lang-*.el).
              # Languages whose grammar is dropped fall back gracefully to
              # their non-ts mode.
              epkgs.treesit-grammars.with-grammars (
                p: with p; [
                  tree-sitter-bash
                  tree-sitter-c
                  tree-sitter-cmake
                  tree-sitter-comment
                  tree-sitter-cpp
                  tree-sitter-css
                  tree-sitter-dockerfile
                  tree-sitter-elisp
                  tree-sitter-go
                  tree-sitter-gomod
                  tree-sitter-gowork
                  tree-sitter-html
                  tree-sitter-javascript
                  tree-sitter-jsdoc
                  tree-sitter-json
                  tree-sitter-make
                  tree-sitter-markdown
                  tree-sitter-markdown-inline
                  tree-sitter-nix
                  tree-sitter-python
                  tree-sitter-regex
                  tree-sitter-rust
                  tree-sitter-toml
                  tree-sitter-tsx
                  tree-sitter-typescript
                  tree-sitter-yaml
                  tree-sitter-zig
                ]
              )
            else
              epkgs.treesit-grammars.with-all-grammars
          )
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
            --suffix INFOPATH : "${final.jotainInfo}/share/info:"
        done
      '';
in
{
  jotainEmacs = import ../emacs.nix {
    pkgs = final;
    inherit variant;
  };

  # Terminal-only (`-nw`) build, used by the nix-on-droid module: Android
  # under proot is headless, so a GUI Emacs would only bloat the closure
  # with unusable X/Wayland libraries.
  jotainEmacsNoGui = import ../emacs.nix {
    pkgs = final;
    inherit variant;
    noGui = true;
  };

  jylhisEmacs = import ../emacs-jylhis.nix (
    {
      pkgs = final;
    }
    // final.lib.optionalAttrs (jylhisEmacsSrc != null) {
      src = jylhisEmacsSrc;
    }
  );

  sonarlintLs = final.sonarlint-ls;

  # Prebuilt ECA server binary for the eca-emacs client (lisp/init-ai.el).
  # Surfaced on the overlay so module-system / Home Manager consumers can put
  # it on the wrapper PATH.
  eca = import ./eca-server.nix { pkgs = final; };

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

  jylhisEmacsPackages = mkJotainEmacsPackages {
    name = "jylhis-emacs-full";
    package = final.jylhisEmacs;
  };
}

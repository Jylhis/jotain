# Extra Emacs Lisp packages not available on any archive (MELPA,
# GNU ELPA, NonGNU ELPA).  Shared between default.nix and devenv.nix.
{ pkgs }:

efinal: eprev:
let
  # Emacs 31 ships newer xref/project/eglot/flymake in-tree, but
  # emacs-overlay's generated package set still publishes them as
  # standalone GNU ELPA derivations. Transitive `Package-Requires' from
  # installed packages (consult-eglot, eglot-tempel, projection, breadcrumb,
  # and ELPA flymake) drag those ELPA copies into the distribution's
  # site-lisp, whose dirs are prepended to `load-path' at startup and
  # SHADOW the in-tree versions. The stale ELPA xref-1.7.0 lacks
  # `global-xref-mouse-mode', so init-prog.el's guarded
  # `(global-xref-mouse-mode 1)' hit a failing autoload and errored at
  # startup (the others are latent shadows of the same kind).
  #
  # Replace each with an empty package so the in-tree copy wins.
  # `trivialBuild' needs at least one .el, so we ship a single inert shim
  # NOT named after the feature: the result is a valid derivation (so the
  # transitive `packageRequires' still resolve) whose site-lisp dir carries
  # no `xref.el'/`project.el'/`eglot.el'/`flymake.el' to shadow the
  # built-in. A `(provide 'xref)' stub must NOT be used: it would
  # re-shadow. Consumers still byte-compile against the base Emacs's
  # in-tree copies (on load-path at build time), which are supersets.
  # A source *directory* (not a bare .el, since trivialBuild's unpackPhase
  # cp's a directory and mis-handles a store path whose name ends in
  # `.el') holding one inert shim file.
  shimSrc =
    name:
    pkgs.runCommand "${name}-builtin-shim-src" { } ''
      mkdir -p "$out"
      cat > "$out/${name}-nixpkgs-builtin-shim.el" <<'SHIM'
      ;;; ${name}-nixpkgs-builtin-shim.el --- use Emacs's in-tree ${name} -*- lexical-binding: t; -*-
      ;;; Commentary:
      ;; Intentionally ships no `${name}.el'.  See nix/extra-packages.nix:
      ;; this replaces the stale GNU ELPA ${name} so Emacs 31's in-tree
      ;; ${name} is not shadowed on load-path.
      ;;; Code:
      (provide '${name}-nixpkgs-builtin-shim)
      ;;; ${name}-nixpkgs-builtin-shim.el ends here
      SHIM
    '';

  emptyElpaPackage =
    name:
    efinal.trivialBuild {
      pname = "${name}-nixpkgs-builtin-shim";
      version = "1";
      src = shimSrc name;
    };
in
{
  # Stop stale GNU ELPA core packages from shadowing Emacs 31's in-tree
  # versions (see `emptyElpaPackage' above).
  xref = emptyElpaPackage "xref";
  project = emptyElpaPackage "project";
  eglot = emptyElpaPackage "eglot";
  flymake = emptyElpaPackage "flymake";

  # TEMPORARY (2026-07-21): emacs-overlay's ghostel epkg builds the
  # libghostty-vt native module with zig, and the module's zig-deps
  # fixed-output fetch is currently unbuildable on GitHub CI runners —
  # zig's HTTP/git fetcher fails deterministically against github.com
  # (HttpConnectionClosing / WriteFailed, three runs on 2026-07-21).
  # Rebuild the package Elisp-only from the same pinned MELPA source so
  # the distribution stays buildable; `ghostel-module-auto-install
  # 'download` (lisp/init-terminal.el) restores the module at runtime.
  # Revert to the plain epkgs.ghostel once the upstream fetch works.
  ghostel = efinal.trivialBuild {
    pname = "ghostel";
    version = eprev.ghostel.version or "0";
    src = eprev.ghostel.src;
    packageRequires = eprev.ghostel.packageRequires or [ ];
    # The MELPA recipe cherry-picks the elisp out of the repo; trivialBuild
    # wants it at the source root, so point sourceRoot at ghostel.el's dir.
    postUnpack = ''
      elFile=$(find "$sourceRoot" -name ghostel.el -print -quit)
      if [ -n "$elFile" ]; then
        sourceRoot=$(dirname "$elFile")
      fi
    '';
  };

  # Pinned in nix/design-pin.nix, shared with the website's vendored CSS so
  # the editor and page.jylhis.com/jotain can never sit on different versions of the
  # design system.  v3.0.0 collapses upstream to a single theme — generated
  # outputs rename from jylhis-{survey,mono}-{light,dark} to jylhis-{light,dark}
  # and are no longer committed (jylhis-themes.el stays a committed source),
  # so the generator runs here in-derivation (bun + sources in), mirroring
  # upstream's own nix/emacs.nix.  trivialBuild globs every *.el, so the
  # rename needed no change beyond the src wiring.
  jylhis-emacs-themes =
    let
      pin = import ./design-pin.nix;
      src = pkgs.fetchFromGitHub {
        inherit (pin)
          owner
          repo
          rev
          sha256
          ;
      };
      generated =
        pkgs.runCommandLocal "jylhis-generated"
          {
            nativeBuildInputs = [ pkgs.bun ];
          }
          ''
            cp -r ${src}/. "$TMP/src/"
            chmod -R u+w "$TMP/src"
            export HOME="$TMPDIR"
            cd "$TMP/src"
            bun scripts/generate.mjs --out "$out"
          '';
    in
    efinal.trivialBuild {
      pname = "jylhis-emacs-themes";
      inherit (pin) version;
      src = pkgs.symlinkJoin {
        name = "jylhis-emacs-themes-src";
        paths = [
          "${generated}/platforms/emacs"
          "${src}/platforms/emacs"
        ];
      };
    };

  claude-code-ide = efinal.trivialBuild {
    pname = "claude-code-ide";
    version = "0.2.6";
    src = pkgs.fetchFromGitHub {
      owner = "manzaltu";
      repo = "claude-code-ide.el";
      rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
      sha256 = "148xcrqff6khpwf8nnadcyvz8h6mk45xz1498k0wbzy80yzd2axn";
    };
    packageRequires = with efinal; [
      websocket
      web-server
    ];
  };

  # Boosts eglot by wrapping local stdio language servers in the
  # emacs-lsp-booster binary (nix/runtime-deps.nix), which converts server
  # JSON into Elisp bytecode Emacs reads directly and buffers I/O.  Wired in
  # lisp/init-prog.el.  Not on MELPA/ELPA; requires only Emacs built-ins
  # (eglot, jsonrpc, seq), so no packageRequires.
  eglot-booster = efinal.trivialBuild {
    pname = "eglot-booster";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "jdtsmith";
      repo = "eglot-booster";
      rev = "510f579409627c333ef0e9157db713b1004da842";
      sha256 = "0g9ld3dbhj7g5wbrbq21pp274givxpavc44zmql5fn7z93ir258y";
    };
  };

  combobulate = efinal.trivialBuild {
    pname = "combobulate";
    version = "0-unstable-2026-01-26";
    src = pkgs.fetchFromGitHub {
      owner = "mickeynp";
      repo = "combobulate";
      rev = "38773810b5e532f25d11c6d1af02c3a8dffeacd7";
      sha256 = "0j647m17bwj4hia32nq650z7bpnxcg5bflk0z8r867qzmg8j6vc1";
    };
  };

  # `project.el' backend for the Nix (and Guix) store, wired in
  # lisp/init-project.el: each /nix/store path that is a directory becomes a
  # project root, so `project-find-file' works while visiting store files.
  # Published on NonGNU ELPA as `project-nix-store', but the pinned
  # emacs-overlay snapshot (2026-08-24) predates the upstream
  # `project-store' -> `project-nix-store' rename (0.10.0), so the base epkgs
  # scope does not carry this name yet; build it from the tagged release.
  # Package-Requires is ((emacs "29.1")) — only built-ins — so no
  # packageRequires.
  project-nix-store = efinal.trivialBuild {
    pname = "project-nix-store";
    version = "0.10.0";
    src = pkgs.fetchFromGitHub {
      owner = "jian-lin";
      repo = "project-nix-store";
      rev = "ae6c743f98f2f46222cec670d96b1d125db1c7c5"; # tag 0.10.0
      sha256 = "184hy7jy1zq97xr4ddgxi2mppj25gv53j0k692mx74lwlbxic5cd";
    };
  };

  # Tree-sitter QML major mode (lisp/init-lang-qml.el), for editing
  # Quickshell / Qt Quick `.qml' files.  Not on MELPA.  Uses the `qmljs'
  # grammar, which the distribution already ships via
  # treesit-grammars.with-all-grammars.  Depends only on Emacs built-ins
  # (treesit, c-ts-common, js), so no packageRequires.
  qml-ts-mode = efinal.trivialBuild {
    pname = "qml-ts-mode";
    version = "0.1";
    src = pkgs.fetchFromGitHub {
      owner = "xhcoding";
      repo = "qml-ts-mode";
      rev = "b80c6663521b4d0083e416e6712ebc02d37b7aec";
      sha256 = "079fj4vm8pyjfm62yba8r089rlhy725qm27b3fj4vx25s44vywjr";
    };
  };

  # Magit-style porcelain for Jujutsu (jj), wired in lisp/init-vc.el.  Not on
  # MELPA.  `evil' is listed even though Jotain doesn't use it: trivialBuild
  # byte-compiles every .el in the source, including majutsu-evil.el whose
  # top-level `(require 'evil)' would otherwise fail to compile.
  majutsu = efinal.trivialBuild {
    pname = "majutsu";
    version = "0.6.0";
    src = pkgs.fetchFromGitHub {
      owner = "0WD0";
      repo = "majutsu";
      rev = "97169be899dca13d08212dc88993fbb7b8272b2f";
      sha256 = "043d3vcd19wzc7hpr20d83kwi5973wii08wfbhp69lab8shzaimn";
    };
    packageRequires = with efinal; [
      compat
      transient
      magit
      evil
    ];
  };

  # Emacs integration for the tagref CLI ([tag:x]/[ref:x] cross-references):
  # completion, xref navigation, and M-x tagref-check.  Not on MELPA; depends
  # only on Emacs built-ins, so no packageRequires.
  tagref = efinal.trivialBuild {
    pname = "tagref";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "vedang";
      repo = "tagref.el";
      rev = "8356b83afee687b1d4011e6dc79716055aa20e7f";
      sha256 = "0zki2d7c4vsaq68s9rac6zcr5q3gagpgdvrcshnkdniw7r8ph8ww";
    };
  };
}

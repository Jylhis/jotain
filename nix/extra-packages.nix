# Extra Emacs Lisp packages not available on any archive (MELPA,
# GNU ELPA, NonGNU ELPA).  Shared between default.nix and devenv.nix.
{ pkgs }:

efinal: eprev: {
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

  jylhis-emacs-themes = efinal.trivialBuild {
    pname = "jylhis-emacs-themes";
    version = "0.4.0-unstable-2026-07-24";
    src =
      pkgs.fetchFromGitHub {
        owner = "Jylhis";
        repo = "design";
        rev = "2b56be9992dbaf647b73817e1d7c691737376a55";
        sha256 = "00kcfp1n7kzy4ma5gddbba9bbc2jw5lix4yczskzc3irphhx7qwy";
      }
      + "/platforms/emacs";
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

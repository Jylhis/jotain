# Extra Emacs Lisp packages not available on any archive (MELPA,
# GNU ELPA, NonGNU ELPA).  Imported by nix/mk-overlay.nix as the
# emacsPackages override, so every overlay consumer (the flake's
# packages, default.nix, and devenv.nix) picks these up.
{ pkgs }:

efinal: _eprev: {
  jylhis-emacs-themes = efinal.trivialBuild {
    pname = "jylhis-emacs-themes";
    version = "0.3.0";
    src =
      pkgs.fetchFromGitHub {
        owner = "Jylhis";
        repo = "design";
        rev = "0e59959881172c7010c051507b2d4f07a18eb26d";
        sha256 = "1qa8zq1lhprvv702rfnc6xjlfrl0mx5ianph2n23k2wx37s157p0";
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

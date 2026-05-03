# Extra Emacs Lisp packages not available on any archive (MELPA,
# GNU ELPA, NonGNU ELPA).  Shared between default.nix and devenv.nix.
{ pkgs }:

efinal: _eprev: {
  jylhis-emacs-themes = efinal.trivialBuild {
    pname = "jylhis-emacs-themes";
    version = "0.3.0";
    src =
      pkgs.fetchFromGitHub {
        owner = "Jylhis";
        repo = "design";
        rev = "4a8326edcc4b22dbe6920a33e0246e9c411e3cb4";
        sha256 = "0qj226pn392x894kdqkfp2gm9wfrqxd93ygxij4w3z0zqplhjvx6";
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
}

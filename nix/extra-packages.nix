# Emacs packages Nix builds itself (absent from every archive), plus
# overrides of archive packages: built-in shims and ghostel. Overlaid onto
# the package scope by nix/mk-overlay.nix.
{ pkgs }:

efinal: eprev:
let
  # Emacs 31 ships newer xref/project/eglot/flymake in-tree, but
  # transitive `Package-Requires' (consult-eglot, eglot-tempel,
  # projection, breadcrumb, ELPA flymake) pull emacs-overlay's standalone
  # GNU ELPA copies into site-lisp, which is prepended to `load-path' and
  # SHADOWS the in-tree versions. (The stale ELPA xref lacks
  # `global-xref-mouse-mode', which broke init-prog.el at startup.)
  #
  # Replace each with an empty package: a valid derivation, so the
  # `packageRequires' still resolve, holding one inert shim NOT named
  # after the feature (`trivialBuild' needs one .el). A `(provide 'xref)'
  # stub would re-shadow. Consumers still byte-compile against the
  # in-tree copies, which are supersets. The shim lives in a source
  # directory: trivialBuild's unpackPhase mis-handles a store path ending
  # in `.el'.
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
  # `emacsWithPackagesFromUsePackage` lists `use-package` itself as a wanted
  # package, which would put the ELPA copy ahead of the in-tree one.
  use-package = emptyElpaPackage "use-package";
  xref = emptyElpaPackage "xref";
  project = emptyElpaPackage "project";
  eglot = emptyElpaPackage "eglot";
  flymake = emptyElpaPackage "flymake";

  # Elisp-only rebuild of emacs-overlay's ghostel: its zig-deps fixed-output
  # fetch failed on GitHub CI runners (zig's fetcher against github.com:
  # HttpConnectionClosing / WriteFailed). Without the native module,
  # auto-install would download into the read-only store path. Revert to
  # the plain epkgs.ghostel once the fetch works there.
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

  # The generated jylhis-{light,dark} themes are not committed upstream, so
  # the generator runs here. nix/ds-assets.nix reads `src` and `version`.
  jylhis-emacs-themes = efinal.trivialBuild {
    pname = "jylhis-emacs-themes";
    version = "3.0.0-unstable-2026-09-23";
    src = pkgs.fetchFromGitHub {
      owner = "Jylhis";
      repo = "design";
      rev = "e24a796d090f56aced9a3f11a8763a3ba586b157";
      hash = "sha256-kEBsdVLVxLDB5ZZBzxt4NY2SqmqskGM9r1y2xFULuZI=";
    };
    nativeBuildInputs = [ pkgs.bun ];
    postUnpack = ''
      export HOME="$TMPDIR"
      (cd "$sourceRoot" && bun scripts/generate.mjs --out "$TMPDIR/generated")
      mkdir emacs-src
      cp -r "$TMPDIR/generated/platforms/emacs/." emacs-src/
      cp -r "$sourceRoot/platforms/emacs/." emacs-src/
      chmod -R u+w emacs-src
      sourceRoot=emacs-src
    '';
  };

  claude-code-ide = efinal.trivialBuild {
    pname = "claude-code-ide";
    # Upstream cuts no tags; the Version header reads 0.3.0 in-dev.
    version = "0.3.0-unstable-2026-09-14";
    src = pkgs.fetchFromGitHub {
      owner = "manzaltu";
      repo = "claude-code-ide.el";
      rev = "50a3d55262805d7207889ed429ff30da96fbf68b";
      sha256 = "0dgccddi71gghjzw23y4d2k61dzjz5p0rvy91cxwgl7168z3pvxv";
    };
    packageRequires = with efinal; [
      websocket
      web-server
    ];
  };

  # Wraps eglot's stdio servers in emacs-lsp-booster
  # (nix/runtime-deps.nix); wired in lisp/init-prog.el. Built-ins only.
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
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "mickeynp";
      repo = "combobulate";
      rev = "69b74248aeaefa06ea0e4da6f93a65b51956820d"; # tag v1.0.0
      sha256 = "0fnqwkd83ll5cbfp86pa20aifns0lm88xn4hm6dkml9kdbkivca5";
    };
  };

  # Tree-sitter QML mode (lisp/init-lang-qml.el), using the bundled
  # `qmljs' grammar. Not on MELPA; built-ins only.
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

  # Emacs integration for the tagref CLI. Not on MELPA; built-ins only.
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

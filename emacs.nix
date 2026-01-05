# Emacs with Jotain packages
{ pkgs
, lib
, devMode ? false
, ...
}:

let
  # Import our lib
  jotainLib = import ./nix/lib { inherit lib pkgs; };

  # Core packages always needed
  corePackages = epkgs: with epkgs; [
    use-package
    no-littering
  ];

  # Get packages from elisp directory if it exists
  autoPackages = epkgs:
    let
      elispDir = ./elisp;
    in
    if builtins.pathExists elispDir then
      jotainLib.dependencies.getPackagesForDirectory elispDir epkgs
    else
      [ ];

  # Development-only packages
  devPackages = epkgs: with epkgs; [
    package-lint
    flycheck
  ];

  # All packages
  allPackages = epkgs:
    corePackages epkgs
    ++ autoPackages epkgs
    ++ (if devMode then devPackages epkgs else [ ]);

  # Use Emacs 30 (or latest from overlay)
  baseEmacs = pkgs.emacs30-pgtk or pkgs.emacs-pgtk or pkgs.emacs;

  # Get runtime dependencies (LSP servers, CLI tools, tree-sitter grammars)
  runtimeDeps = jotainLib.runtimeDeps;

  # Emacs with packages
  emacsWithPackages = (pkgs.emacsPackagesFor baseEmacs).emacsWithPackages allPackages;

  # Create tree-sitter grammar directory
  # Tree-sitter grammars need to be in a directory structure where each
  # grammar is a .so file with the correct naming convention
  treeSitterDir = pkgs.runCommand "treesit-grammars" { } ''
    mkdir -p $out
    ${lib.concatMapStringsSep "\n" (grammar: ''
      # Each grammar package has a 'parser' executable which is the .so file
      if [ -f "${grammar}/parser" ]; then
        # Extract language name from the package name
        # Package names are like: "tree-sitter-bash-grammar-0.25.10"
        # We want just "bash"
        fullname=$(basename "${grammar}")
        # Remove hash prefix if present (e.g., "abc123-tree-sitter-bash-grammar-0.25.10")
        fullname=''${fullname#*-}
        # Extract language name (everything between "tree-sitter-" and version number)
        # Handles both old format (tree-sitter-bash-grammar-X.Y.Z) and new format (tree-sitter-bash-X.Y.Z)
        lang=$(echo "$fullname" | sed -E 's/tree-sitter-([a-z]+)(-grammar)?-[0-9].*/\1/')

        # Copy with the expected name pattern
        cp "${grammar}/parser" "$out/libtree-sitter-$lang.so"
      fi
    '') runtimeDeps.treeSitterGrammars}
  '';

in
# Wrap Emacs with runtime dependencies in PATH
pkgs.symlinkJoin {
  name = "emacs-with-runtime-${baseEmacs.version}";
  paths = [ emacsWithPackages ];

  nativeBuildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    # Wrap emacs binary
    wrapProgram $out/bin/emacs \
      --prefix PATH : ${lib.makeBinPath runtimeDeps.allRuntimeDeps} \
      --prefix TREE_SITTER_DIR : "${treeSitterDir}"

    # Wrap emacsclient if it exists
    if [ -e $out/bin/emacsclient ]; then
      wrapProgram $out/bin/emacsclient \
        --prefix PATH : ${lib.makeBinPath runtimeDeps.allRuntimeDeps}
    fi
  '';

  passthru = (emacsWithPackages.passthru or { }) // {
    # Expose runtime dependencies for home-manager module
    inherit (runtimeDeps) cliTools lspServers treeSitterGrammars allRuntimeDeps fonts;
    unwrapped = emacsWithPackages;
  };

  meta = (emacsWithPackages.meta or { }) // {
    description = "${emacsWithPackages.meta.description or "Emacs"} with runtime dependencies";
  };
}

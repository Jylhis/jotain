# Automatic dependency extraction from Emacs Lisp use-package declarations
{ lib, pkgs }:

let
  # Extract use-package declarations from an elisp file using pure Nix
  extractUsePackages = elispFile:
    let
      content = builtins.readFile elispFile;

      # Split content into lines
      lines = lib.splitString "\n" content;

      # Extract package names from use-package declarations
      # This is a simplified regex-based approach that works in pure Nix
      extractPackageName = line:
        let
          # Match (use-package PACKAGE-NAME
          match = builtins.match "[[:space:]]*\\(use-package[[:space:]]+([a-zA-Z0-9-]+).*" line;
        in
        if match != null then builtins.head match else null;

      # Check if a block contains :ensure nil or :nodep
      # We need to find the matching closing paren for this use-package
      shouldSkip = pkgName: lineIdx:
        let
          # Get the use-package block by counting parentheses
          # Start with 1 open paren (from the (use-package line)
          getBlock = idx: depth: accum:
            if idx >= (builtins.length lines) || depth == 0 then
              accum
            else
              let
                line = builtins.elemAt lines idx;
                # Count opening and closing parens
                opens = lib.length (lib.filter (c: c == "(") (lib.stringToCharacters line));
                closes = lib.length (lib.filter (c: c == ")") (lib.stringToCharacters line));
                newDepth = depth + opens - closes;
                newAccum = accum + " " + line;
              in
              getBlock (idx + 1) newDepth newAccum;

          # Get the full use-package block text
          blockText = getBlock (lineIdx + 1) 1 (builtins.elemAt lines lineIdx);

          # Check for :ensure nil or :nodep in the block
          hasEnsureNil = builtins.match ".*:ensure[[:space:]]+nil.*" blockText != null;
          hasNodep = builtins.match ".*:nodep.*" blockText != null;
        in
        hasEnsureNil || hasNodep;

      # Extract packages from all lines
      packagesWithIndex = lib.imap0
        (idx: line:
          let pkgName = extractPackageName line;
          in if pkgName != null && !(shouldSkip pkgName idx) then pkgName else null
        )
        lines;

      # Filter out nulls
      packages = builtins.filter (x: x != null) packagesWithIndex;
    in
    packages;

  # Scan directory for all .el files and extract packages
  scanDirectory = dir:
    let
      # Find all .el files
      elispFiles = lib.filesystem.listFilesRecursive dir;
      elFiles = builtins.filter (f: lib.hasSuffix ".el" (toString f)) elispFiles;

      # Extract packages from each file
      allPackages = lib.lists.flatten (map extractUsePackages elFiles);

      # Remove duplicates and empty strings
      uniquePackages = lib.lists.unique (builtins.filter (x: x != "") allPackages);
    in
    uniquePackages;

  # Map elisp package names to nixpkgs emacsPackages names
  # Only needed for exceptions: packages with different names or that should be excluded
  # Most packages work automatically via the `or pkgName` fallback in mapToNixpkgsName
  packageNameMap = {
    # Built-in packages (exclude from installation)
    "project" = null; # Built-in to Emacs 30+
    "diff-mode" = null; # Built-in to Emacs
    "xt-mouse" = null; # Built-in to Emacs

    # Local libraries (exclude from installation)
    "app-launcher" = null; # Local file in elisp/app-launcher.el
  };

  # Map elisp package name to nixpkgs name
  mapToNixpkgsName = pkgName:
    packageNameMap.${pkgName} or pkgName;

  # Convert package name to Emacs package attribute
  # Returns null if package doesn't exist or is built-in
  toEmacsPackage = epkgs: name:
    let
      nixName = mapToNixpkgsName name;
    in
    if nixName == null then
      null  # Built-in package, skip
    else
      epkgs.${nixName} or
        (builtins.trace "Warning: Package '${name}' (nixpkgs: '${nixName}') not found in emacsPackages" null);

  # Get all Emacs packages needed from a directory of elisp files
  getPackagesForDirectory = elispDir: epkgs:
    let
      packageNames = scanDirectory elispDir;
      nixPackages = map (toEmacsPackage epkgs) packageNames;
      # Filter out nulls (built-in or missing packages)
      validPackages = builtins.filter (x: x != null) nixPackages;
    in
    validPackages;

  # Generate a list of package names (for documentation/debugging)
  listPackageNames = elispDir:
    let
      packageNames = scanDirectory elispDir;
      mapped = map (name: "${name} -> ${mapToNixpkgsName name}") packageNames;
    in
    lib.concatStringsSep "\n" mapped;

in
{
  inherit
    extractUsePackages
    scanDirectory
    mapToNixpkgsName
    toEmacsPackage
    getPackagesForDirectory
    listPackageNames;
}

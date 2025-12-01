# Automatic dependency extraction from Emacs Lisp use-package declarations
{ lib, pkgs }:

let
  # Manual overrides for complex cases or files that need explicit control
  # Only add entries here if the pure Nix extraction fails for a specific file
  packageOverrides = {
    # No overrides needed - pure Nix extraction works correctly
  };

  # Extract use-package declarations from an elisp file using pure Nix regex
  # No derivations = no cross-system build issues
  extractUsePackages = elispFile:
    let
      fileName = baseNameOf elispFile;
    in
    if packageOverrides ? ${fileName} then
    # Use manual override if available
      packageOverrides.${fileName}
    else
    # Otherwise use pure Nix extraction
      let
        content = builtins.readFile elispFile;

        # Split by (use-package PACKAGE-NAME
        # Returns list alternating between strings and capture groups
        parts = builtins.split "\\(use-package[[:space:]]+([a-zA-Z0-9-]+)" content;

        # Process each match with its index
        processBlock = idx: item:
          if builtins.isList item then
            let
              pkgName = builtins.head item; # Package name from capture group

              # Get the next part (block content after package name)
              # This contains the rest of the use-package declaration
              nextIdx = idx + 1;
              nextPart =
                if nextIdx < builtins.length parts
                then builtins.elemAt parts nextIdx
                else "";

              # Check for :ensure nil or :nodep in the block
              # We look at a reasonable chunk (next part until next use-package or end)
              hasEnsureNil = builtins.match ".*:ensure[[:space:]]+nil.*" nextPart != null;
              hasNoDep = builtins.match ".*:nodep.*" nextPart != null;
            in
            if hasEnsureNil || hasNoDep then null else pkgName
          else
            null;

        # Process all parts with their indices
        results = lib.imap0 (idx: item: processBlock idx item) parts;

        # Filter out nulls and empty strings
        validPackages = builtins.filter (x: x != null && x != "") results;
      in
      validPackages;

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

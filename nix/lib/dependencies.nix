# Automatic dependency extraction from Emacs Lisp use-package declarations
{ lib, pkgs }:

let
  # Extract use-package declarations from an elisp file using regex
  extractUsePackages = elispFile:
    let
      content = builtins.readFile elispFile;

      # Use perl to extract use-package declarations
      # Matches (use-package PACKAGE-NAME ...) but excludes those with :ensure nil or :nodep
      extracted = pkgs.runCommand "extract-packages-${baseNameOf elispFile}"
        {
          buildInputs = [ pkgs.perl ];
          inherit content;
        } ''
        echo "$content" | perl -ne '
          # Match (use-package package-name
          if (/\(use-package\s+([a-zA-Z0-9-]+)/) {
            my $pkg = $1;
            my $line = $_;

            # Read ahead to check for :ensure nil or :nodep
            my $block = $_;
            my $paren_count = 1;
            while ($paren_count > 0 && defined($line = <>)) {
              $block .= $line;
              $paren_count += ($line =~ tr/\(//);
              $paren_count -= ($line =~ tr/\)//);
            }

            # Skip if :ensure nil or :nodep is present
            unless ($block =~ /:ensure\s+nil/ || $block =~ /:nodep/) {
              print "$pkg\n";
            }
          }
        ' > $out
      '';
    in
    lib.splitString "\n" (lib.removeSuffix "\n" (builtins.readFile extracted));

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

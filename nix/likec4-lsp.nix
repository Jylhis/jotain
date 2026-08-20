# LikeC4 language server (`likec4-lsp`), the standalone `@likec4/lsp` npm
# package (https://likec4.dev/tooling/editors/#emacs).
#
# Backs `likec4-mode`'s eglot LSP (lisp/init-lang-devops.el +
# init-prog.el's `jotain-prog--likec4-server`). Bundled onto every
# distribution wrapper's PATH via nix/runtime-deps.nix, so `just
# run-built` and the module/daemon installs get diagnostics/completion for
# `.c4'/`.likec4' files with no user npm install.
#
# Not in nixpkgs, so packaged from the npm registry with buildNpmPackage.
# The published tarball ships no lockfile, so `nix/likec4-lsp/{package.json,
# package-lock.json}` vendors a wrapper pinning `@likec4/lsp@1.59.2`; the
# lock was generated with
#   nix shell nixpkgs#nodejs_22 --command \
#     npm install --package-lock-only --ignore-scripts
# and the hash below with `nix run nixpkgs#prefetch-npm-deps -- \
# nix/likec4-lsp/package-lock.json`. Re-run both on a version bump.
#
# Despite the docs' "zero dependencies" claim the package pulls esbuild
# (via bundle-require). We skip esbuild's platform-binary postinstall
# (--ignore-scripts) and point both the build and the runtime wrapper at
# nixpkgs' esbuild through ESBUILD_BINARY_PATH.
{ pkgs }:
let
  inherit (pkgs) lib;
  nodejs = pkgs.nodejs_22;
in
pkgs.buildNpmPackage {
  pname = "likec4-lsp";
  version = "1.59.2";

  src = ./likec4-lsp;

  npmDepsHash = "sha256-dGhd1MDk8rGRJfd+9Ud0BOlIMh9CSnAq1J5P+BGoZ/M=";

  inherit nodejs;

  # Prebuilt JS from the registry — nothing to compile.
  dontNpmBuild = true;
  # esbuild's postinstall downloads a platform binary; skip it and use
  # nixpkgs' esbuild at build and run time instead.
  npmFlags = [ "--ignore-scripts" ];
  ESBUILD_BINARY_PATH = "${pkgs.esbuild}/bin/esbuild";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  # The vendored wrapper package declares no `bin`; the real entry point is
  # the `likec4-lsp` bin from the @likec4/lsp dependency. Install the
  # resolved node_modules and wrap that entry with node + esbuild.
  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib $out/bin
    cp -r node_modules $out/lib/node_modules
    makeWrapper ${nodejs}/bin/node $out/bin/likec4-lsp \
      --add-flags "$out/lib/node_modules/@likec4/lsp/bin/likec4-lsp.mjs" \
      --set ESBUILD_BINARY_PATH "${pkgs.esbuild}/bin/esbuild"
    runHook postInstall
  '';

  meta = {
    description = "LikeC4 language server (@likec4/lsp) for architecture-as-code files";
    homepage = "https://likec4.dev/tooling/editors/";
    license = lib.licenses.mit;
    mainProgram = "likec4-lsp";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    sourceProvenance = [ lib.sourceTypes.fromSource ];
  };
}

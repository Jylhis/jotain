# LikeC4 language server (`likec4-lsp`), the standalone `@likec4/lsp` npm
# package (https://likec4.dev/tooling/editors/#emacs).
#
# Backs `likec4-mode`'s eglot server (lisp/init-lang-devops.el,
# init-prog.el's `jotain-prog--likec4-server`); on every distribution
# wrapper's PATH via nix/runtime-deps.nix.
#
# Not in nixpkgs, and the npm tarball ships no lockfile, so
# nix/likec4-lsp/ vendors a wrapper package.json + package-lock.json
# pinning `@likec4/lsp@1.59.4`. `just update-pins likec4-lsp` regenerates
# the lock (`npm install --package-lock-only --ignore-scripts`) and the
# hash (`prefetch-npm-deps`).
#
# The package pulls esbuild (via bundle-require). Its platform-binary
# postinstall is skipped and nixpkgs' esbuild used via ESBUILD_BINARY_PATH.
{ pkgs }:
let
  inherit (pkgs) lib;
  nodejs = pkgs.nodejs_22;
in
pkgs.buildNpmPackage {
  pname = "likec4-lsp";
  version = "1.59.4";

  src = ./likec4-lsp;

  npmDepsHash = "sha256-l2NrBh1O3P/oKy9DpW5XER7OlZq4GZXmQyoxrTcwJGM=";

  inherit nodejs;

  # Prebuilt JS from the registry — nothing to compile.
  dontNpmBuild = true;
  npmFlags = [ "--ignore-scripts" ];
  ESBUILD_BINARY_PATH = "${pkgs.esbuild}/bin/esbuild";

  nativeBuildInputs = [ pkgs.makeWrapper ];

  # The vendored wrapper has no `bin`; wrap @likec4/lsp's entry point.
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

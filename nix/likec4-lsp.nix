# LikeC4 language server: the standalone `@likec4/lsp` npm package, for
# `jotain-likec4-mode` (lisp/init-lang-devops.el).
#
# The npm tarball ships no lockfile, so nix/likec4-lsp/ vendors a wrapper
# package.json and package-lock.json. `nix-update --use-update-script
# likec4-lsp` bumps both; the version below is read from package.json.
{ pkgs }:
let
  inherit (pkgs) lib;
  nodejs = pkgs.nodejs_22;
  inherit (builtins.fromJSON (builtins.readFile ./likec4-lsp/package.json)) version;
in
pkgs.buildNpmPackage {
  pname = "likec4-lsp";
  inherit version;

  src = ./likec4-lsp;

  npmDeps = pkgs.importNpmLock { npmRoot = ./likec4-lsp; };
  inherit (pkgs.importNpmLock) npmConfigHook;

  inherit nodejs;

  dontNpmBuild = true;
  npmFlags = [ "--ignore-scripts" ];
  # esbuild's platform-binary postinstall is skipped; use nixpkgs' binary.
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

  passthru.updateScript = lib.getExe (
    pkgs.writeShellApplication {
      name = "update-likec4-lsp";
      runtimeInputs = [
        pkgs.nodejs_22
        pkgs.jq
        pkgs.moreutils
      ];
      text = ''
        dir=nix/likec4-lsp
        latest=$(npm view @likec4/lsp version)
        jq --arg v "$latest" '.version = $v | .dependencies["@likec4/lsp"] = $v' "$dir/package.json" | sponge "$dir/package.json"
        rm -f "$dir/package-lock.json"
        (cd "$dir" && npm install --package-lock-only --ignore-scripts)
      '';
    }
  );

  meta = {
    description = "LikeC4 language server (@likec4/lsp) for architecture-as-code files";
    homepage = "https://likec4.dev/tooling/editors/";
    license = lib.licenses.mit;
    mainProgram = "likec4-lsp";
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.fromSource ];
  };
}

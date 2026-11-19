# Prebuilt Editor Code Assistant (ECA) server for lisp/init-ai.el.
{ pkgs }:
let
  inherit (pkgs) lib stdenv;
in
stdenv.mkDerivation (finalAttrs: {
  pname = "eca";
  version = "0.161.2";

  src = pkgs.fetchurl {
    url = "https://github.com/editor-code-assistant/eca/releases/download/${finalAttrs.version}/eca-native-static-linux-amd64.zip";
    hash = "sha256-zTQwzxJxpwwVdFtVJQGg4PSbyIEB4+z+XbWplZbf6ZY=";
  };

  nativeBuildInputs = [ pkgs.unzip ];

  sourceRoot = ".";
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 eca "$out/bin/eca"
    runHook postInstall
  '';

  meta = {
    description = "Editor Code Assistant (ECA) server - AI pair-programming backend";
    homepage = "https://github.com/editor-code-assistant/eca";
    license = lib.licenses.asl20;
    mainProgram = "eca";
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
})

# Prebuilt Editor Code Assistant (ECA) server binary.
#
# For the eca-emacs client (lisp/init-ai.el), which otherwise downloads
# the server on first `M-x eca'. It finds `eca' on $PATH, so no
# `eca-custom-command' is needed.
{ pkgs }:
let
  inherit (pkgs) lib stdenv;

  version = "0.161.2";

  baseUrl = "https://github.com/editor-code-assistant/eca/releases/download/${version}";

  # The hash is the asset's `.sha256' sidecar (scripts/update-pins.sh).
  # The static build needs no patching.
  asset = "eca-native-static-linux-amd64.zip";
  sha256 = "cd3430cf1271a70c15745b552501a0e0f49bc88101e3ecfe5db5a99596dfe996";
in
stdenv.mkDerivation {
  pname = "eca";
  inherit version;

  src = pkgs.fetchurl {
    url = "${baseUrl}/${asset}";
    inherit sha256;
  };

  nativeBuildInputs = [ pkgs.unzip ];

  # The archive holds a single native binary named `eca'.
  sourceRoot = ".";
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 eca "$out/bin/eca"
    runHook postInstall
  '';

  meta = {
    description = "Editor Code Assistant (ECA) server — AI pair-programming backend";
    homepage = "https://github.com/editor-code-assistant/eca";
    license = lib.licenses.asl20;
    mainProgram = "eca";
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}

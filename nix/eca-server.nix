# Prebuilt Editor Code Assistant (ECA) server binary.
#
# The eca-emacs client (lisp/init-ai.el) talks to this server over JSONRPC.
# eca-emacs would otherwise download the server on first `M-x eca'; pinning it
# here keeps the dev shell reproducible and offline-capable. eca-emacs
# auto-detects `eca' on $PATH, so no `eca-custom-command' is needed.
{ pkgs }:
let
  inherit (pkgs) lib stdenv;

  version = "0.136.2";

  baseUrl = "https://github.com/editor-code-assistant/eca/releases/download/${version}";

  # Host platform -> { asset, sha256 }. Hashes are the hex sha256 from each
  # release asset's `.sha256' sidecar. x86_64-linux uses the static build so no
  # autoPatchelf is needed; aarch64-linux is dynamic and gets patched below.
  sources = {
    x86_64-linux = {
      asset = "eca-native-static-linux-amd64.zip";
      sha256 = "863b71c1b5ec97a77f0c33db99307401f623d5f05fbaa97ff29b7c2f3c860fdf";
    };
    aarch64-linux = {
      asset = "eca-native-linux-aarch64.zip";
      sha256 = "56d620d0535cb30cc01391a95b8f127976cc7ca6d51810f1d4428cb29b3f1108";
    };
    aarch64-darwin = {
      asset = "eca-native-macos-aarch64.zip";
      sha256 = "4e3f5e974d224109f7986228ff613d2feec1277b943c3259f402d7a875478ba2";
    };
    x86_64-darwin = {
      asset = "eca-native-macos-amd64.zip";
      sha256 = "128d4bc92ce4c4b3f5e90412429a49b5f2ca14d0e1746d696b0716492b42b08e";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
      or (throw "eca-server: unsupported system ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "eca";
  inherit version;

  src = pkgs.fetchurl {
    url = "${baseUrl}/${source.asset}";
    inherit (source) sha256;
  };

  nativeBuildInputs = [
    pkgs.unzip
  ]
  ++ lib.optional stdenv.isLinux pkgs.autoPatchelfHook;

  buildInputs = lib.optionals stdenv.isLinux [
    stdenv.cc.cc.lib
    pkgs.zlib
  ];

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
    platforms = builtins.attrNames sources;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}

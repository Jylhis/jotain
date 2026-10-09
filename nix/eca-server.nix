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

  # Hashes are each asset's `.sha256' sidecar (scripts/update-pins.sh).
  # x86_64-linux uses the static build; aarch64-linux is dynamic and
  # gets autoPatchelf below.
  sources = {
    x86_64-linux = {
      asset = "eca-native-static-linux-amd64.zip";
      sha256 = "cd3430cf1271a70c15745b552501a0e0f49bc88101e3ecfe5db5a99596dfe996";
    };
    aarch64-linux = {
      asset = "eca-native-linux-aarch64.zip";
      sha256 = "86656ad949ee647fac9b80ebf55ea9b8088472d1ed311d3367d7dbb1194bcbf1";
    };
    aarch64-darwin = {
      asset = "eca-native-macos-aarch64.zip";
      sha256 = "7e8aec7c4964d56a0772cb2015b22479a74be3bec5844e94ea9fc118c1881ff4";
    };
    x86_64-darwin = {
      asset = "eca-native-macos-amd64.zip";
      sha256 = "d6fcaa9c67106573b1d9a633004fe67bff324ceb9ed2ebc655807c41529b9298";
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
  ++ lib.optional stdenv.hostPlatform.isLinux pkgs.autoPatchelfHook;

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
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

{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}:
let
  version = "0.1.0";
  sources = {
    x86_64-linux = {
      url = "https://github.com/elisp-lsp/Ellsp/releases/download/${version}/ellsp_linux-x64.tar.gz";
      hash = "sha256-MGoX+TeI2Wd/vPWHIPbKbTkwYlq2KlhXjWh90lMoH+o=";
    };
    aarch64-linux = {
      url = "https://github.com/elisp-lsp/Ellsp/releases/download/${version}/ellsp_linux-arm64.tar.gz";
      hash = "sha256-xylI6zqTZMBq5yIh4TB6TiuV58m5viBoWSF7v+o3ha0=";
    };
    x86_64-darwin = {
      url = "https://github.com/elisp-lsp/Ellsp/releases/download/${version}/ellsp_macos-x64.tar.gz";
      hash = "sha256-ubSTJIESo66h7LpZ1kCALKJqi38pm1ZJbespDVwRHFw=";
    };
    aarch64-darwin = {
      url = "https://github.com/elisp-lsp/Ellsp/releases/download/${version}/ellsp_macos-arm64.tar.gz";
      hash = "sha256-935I/sJU9vnUYDS+/p2NsneXcamCQMIyC41QR4B1D9w=";
    };
  };
  src =
    sources.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "ellsp";
  inherit version;

  src = fetchurl { inherit (src) url hash; };

  sourceRoot = ".";

  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp ellsp $out/bin/
    chmod +x $out/bin/ellsp
    runHook postInstall
  '';

  meta = {
    description = "Elisp Language Server";
    homepage = "https://github.com/elisp-lsp/Ellsp";
    license = lib.licenses.gpl3Plus;
    mainProgram = "ellsp";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  };
}

# rassumfrassum (`rass`): LSP multiplexer letting eglot drive several
# servers per buffer (init-prog.el, TS/TSX and Python). Not in nixpkgs,
# so built from PyPI.
{ pkgs }:
pkgs.python3Packages.buildPythonApplication rec {
  pname = "rassumfrassum";
  version = "0.3.3";
  pyproject = true;
  build-system = [ pkgs.python3Packages.setuptools ];
  src = pkgs.fetchPypi {
    inherit pname version;
    hash = "sha256-Gs2Qgwafj9m1tdVcw1k4UXTbxgbS5awTCINBkb5HIhc=";
  };
  meta = {
    description = "LSP/JSONRPC multiplexer for connecting one LSP client to multiple servers";
    homepage = "https://github.com/joaotavora/rassumfrassum";
    license = pkgs.lib.licenses.gpl3Plus;
    mainProgram = "rass";
  };
}

{
  fetchFromGitHub,
  buildNpmPackage,
  nodejs,
  writeShellScriptBin,
  emacsPackagesFor,
  emacs,
}:

let
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "elisp-lsp";
    repo = "Ellsp";
    rev = version;
    hash = "sha256-JlM7PeQyloYORA4CJgz+cbChrxWjEE7NJgvsHIjIUIY=";
  };

  epkgs = emacsPackagesFor emacs;

  msgu = epkgs.trivialBuild {
    pname = "msgu";
    version = "0.1.0";
    src = fetchFromGitHub {
      owner = "jcs-elpa";
      repo = "msgu";
      rev = "0.1.0";
      hash = "sha256-Jnse1nX+NtGbN39P/ug3PdB8/PdrMITFwACPHueOeZU=";
    };
  };

  ellspEl = epkgs.trivialBuild {
    pname = "ellsp";
    inherit version;
    inherit src;
    packageRequires = with epkgs; [
      lsp-mode
      company
      log4e
      dash
      s
      msgu
    ];
  };

  emacsWithEllsp = epkgs.withPackages (_: [
    ellspEl
  ]);

  proxy = buildNpmPackage {
    pname = "ellsp-proxy";
    inherit version src;

    sourceRoot = "${src.name}/proxy";

    npmDepsHash = "sha256-9aIlBRFFgBcXdWfHYarWNSYLV9LPsxytBUr8xPmEp1o=";

    dontNpmBuild = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/ellsp-proxy
      cp start.js env.js util.js $out/lib/ellsp-proxy/
      cp -r node_modules $out/lib/ellsp-proxy/
      runHook postInstall
    '';
  };
in
writeShellScriptBin "ellsp" ''
  export ELLSP_EMACS="${emacsWithEllsp}/bin/emacs"
  exec ${nodejs}/bin/node ${proxy}/lib/ellsp-proxy/start.js "$@"
''

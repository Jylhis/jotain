# Jotain main package
{ pkgs
, lib
, stdenv
, ...
}:

let
  jotainEmacs = pkgs.callPackage ../emacs { devMode = false; };

  # CLI wrapper
  jotainCLI = pkgs.writeShellScriptBin "jot" ''
    #!/usr/bin/env bash

    export JOTAIN_HOME="${placeholder "out"}"
    export JOTAIN_ELISP_DIR="${placeholder "out"}/share/emacs/site-lisp/jotain"

    # Use installed Emacs
    export PATH="${jotainEmacs}/bin:$PATH"

    # Run CLI commands
    exec "${placeholder "out"}/libexec/jotain/jot-impl" "$@"
  '';

in
stdenv.mkDerivation {
  pname = "jotain";
  version = "0.1.0";

  src = lib.cleanSource ../../..;

  buildInputs = [ jotainEmacs ];

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/jotain
    mkdir -p $out/libexec/jotain
    mkdir -p $out/bin

    # Install elisp files if they exist
    if [ -d elisp ]; then
      cp -r elisp/* $out/share/emacs/site-lisp/jotain/
    fi

    # Install CLI if it exists
    if [ -d cli ]; then
      cp -r cli/* $out/libexec/jotain/
      chmod +x $out/libexec/jotain/jot 2>/dev/null || true
      # Create symlink to main CLI
      if [ -f $out/libexec/jotain/jot ]; then
        ln -s $out/libexec/jotain/jot $out/bin/jot
      fi
    fi

    # Install templates if they exist
    if [ -d templates ]; then
      mkdir -p $out/share/jotain/templates
      cp -r templates/* $out/share/jotain/templates/
    fi
  '';

  meta = with lib; {
    description = "A NixOS-native Emacs distribution";
    homepage = "https://github.com/Jylhis/jotain";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
    maintainers = [ ];
  };
}

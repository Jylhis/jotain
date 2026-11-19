# nix/user-programs.nix: the programs a user runs for Jotain: the wrapped
# `emacs`, the EDITOR and VISUAL scripts, `jotctl` and the byte-compiled
# config. One definition for module.nix (Home Manager) and for module-system.nix
# (services.jotain.users.<user>, installed in the user's profile), so a host that
# builds them in NixOS and one that builds them in Home Manager get the same store paths.
{
  pkgs,
  pkgsWithOverlay,
  # The selected Emacs distribution (cfg.package or jotainEmacsPackages).
  package,
  # Where the config is deployed; --init-directory is pinned here so a stray
  # ~/.emacs.d/ is never picked up. null leaves the flag off.
  initDirectory,
  # An evaluated services.jotain (or per-user) config with the options of
  # nix/module-options.nix.
  cfg,
}:
let
  inherit (pkgs) lib;
  nativeCompile = cfg.nativeCompile.enable or false;

  # Shared runtime binaries (nix/runtime-deps.nix) plus the opt-in tools,
  # prepended to PATH in emacsWrapper, since the systemd user
  # service does not inherit the login-shell PATH.
  runtimeDeps =
    import ./runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional cfg.sonarlint.enable pkgs.sonarlint-ls
    ++ lib.optional cfg.dockerfileLsp.enable pkgs.dockerfile-language-server
    ++ lib.optional cfg.onePassword.enable pkgs._1password-cli
    ++ lib.optional cfg.sops.enable pkgs.sops
    ++ lib.optional cfg.claudeCode.enable pkgs.claude-code;

  # Byte-compiled config, so the daemon loads .elc instead of
  # interpreting .el (and deferred native compilation, which never fires
  # for plain .el loads, can fill var/eln-cache).
  #
  # Same derivation as nix/checks.nix' `elisp-compile', so on the default
  # configuration `home-manager switch' substitutes CI's cachix artifact.
  # A custom `services.jotain.package' or nixpkgs pin builds it locally.
  # `.core' is the inner emacsWithPackages result (see
  # nix/config-compiled.nix); the `or' covers a custom package.
  compiledConfig = import ./config-compiled.nix {
    inherit pkgs nativeCompile;
    emacs = package.core or package;
  };

  runtimePath = lib.makeBinPath runtimeDeps;

  aspellConf = import ./spell-conf.nix {
    inherit pkgs;
    dictionaries = cfg.spell.dictionaries;
  };

  # `emacs` with --init-directory set, for the daemon and any interactive
  # invocation.
  emacsWrapper = pkgs.writeShellScriptBin "emacs" ''
    export PATH=${runtimePath}''${PATH:+:$PATH}
    ${lib.optionalString (aspellConf != null) ''
      export ASPELL_CONF=${lib.escapeShellArg aspellConf}
    ''}
    ${lib.optionalString nativeCompile ''
      export JOTAIN_ELN_PATH=${compiledConfig}/share/emacs/native-lisp
    ''}
    exec ${package}/bin/emacs ${
      lib.optionalString (initDirectory != null) "--init-directory=${lib.escapeShellArg initDirectory} "
    }"$@"
  '';

  # EDITOR fallback without a daemon; via emacsWrapper to keep
  # --init-directory.
  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${emacsWrapper}/bin/emacs -nw -- "$@"
  '';
in
{
  inherit
    package
    compiledConfig
    emacsWrapper
    runtimeDeps
    ;

  # The wrapper first, so bin/emacs is the wrapper and emacsclient comes from
  # the package; services.emacs reads both from one package.
  daemonPackage = pkgs.symlinkJoin {
    name = "jotain-emacs-daemon";
    paths = [
      emacsWrapper
      package
    ];
  };

  # Colour-emoji fallback for the `emoji' / `symbol' fontsets
  # (lisp/init-ui.el), and the faces `jotain-font-preferences' names: BlexMono
  # for the nerd-icons stack, Fira Sans (variable-pitch), Jost and IBM Plex Mono
  # from the design system.
  fontPackages = [
    pkgs.noto-fonts-color-emoji
    pkgs.nerd-fonts.blex-mono
    pkgs.jost
    pkgs.fira-sans
    pkgs.ibm-plex
  ];

  # EDITOR: terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin package}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      -- \
      "$@"
  '';

  # VISUAL: opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin package}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${emacsWrapper}/bin/emacs \
      -- \
      "$@"
  '';

  # `jotctl <start|stop|status|restart|logs>`: drives services.emacs.
  jotctlScript = pkgs.writeShellScriptBin "jotctl" ''
    units=emacs.service
    systemctl --user cat emacs.socket >/dev/null 2>&1 && units="emacs.socket emacs.service"
    case "''${1:-status}" in
      start)   exec systemctl --user start emacs.service ;;
      stop)    exec systemctl --user stop $units ;;
      restart) exec systemctl --user restart $units ;;
      status)  exec systemctl --user status emacs.service ;;
      logs)    exec journalctl --user -u emacs.service -f ;;
      *) echo "usage: jotctl {start|stop|status|restart|logs}" >&2; exit 2 ;;
    esac
  '';
}

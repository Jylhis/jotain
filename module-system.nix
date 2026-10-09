# module-system.nix — NixOS / nix-darwin module for Jotain Emacs.
#
# Applies the overlay and adds Jotain Emacs to environment.systemPackages;
# one file serves both NixOS and nix-darwin. For a per-user daemon, use
# the Home Manager module (module.nix).
#
# Usage in NixOS:
#
#   imports = [ jotain.nixosModules.default ];
#   services.jotain.enable = true;
#
# Usage in nix-darwin:
#
#   imports = [ jotain.darwinModules.default ];
#   services.jotain.enable = true;
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;
  jotainOverlay = args.jotainOverlay or (import ./overlay.nix);
  pkgsWithOverlay = pkgs.extend jotainOverlay;
  selectedPackage = if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackages;

  # Shared runtime binaries plus the opt-in tools (mirrors module.nix).
  runtimeDeps =
    import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional cfg.devenv.enable pkgs.devenv
    ++ lib.optional cfg.sonarlint.enable pkgs.sonarlint-ls
    ++ lib.optional cfg.dockerfileLsp.enable pkgs.dockerfile-language-server
    ++ lib.optional cfg.onePassword.enable pkgs._1password-cli
    ++ lib.optional cfg.sops.enable pkgs.sops
    ++ lib.optional cfg.claudeCode.enable pkgs.claude-code;

  wrappedPackage = import ./nix/wrap-runtime-deps.nix {
    inherit pkgs runtimeDeps;
    package = selectedPackage;
  };

  # EDITOR: terminal emacsclient.
  editorScript = import ./nix/editor-script.nix {
    inherit pkgs;
    package = wrappedPackage;
  };

  # VISUAL: GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin wrappedPackage}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${lib.getBin wrappedPackage}/bin/emacs \
      -- \
      "$@"
  '';
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Jotain Emacs package to use. Leave unset for the full
        distribution (`jotainEmacsPackages`).
      '';
    };

    defaultEditor = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = ''
        Whether to configure {command}`emacsclient` as the default
        editor using the {env}`EDITOR` and {env}`VISUAL`
        environment variables.
      '';
    };

    sonarlint = {
      enable = lib.mkEnableOption "SonarLint language server ({command}`M-x jotain-sonarlint`)";
    };

    devenv = {
      enable = lib.mkEnableOption ''
        the {command}`devenv` CLI on the wrapper PATH, for the native
        environment loader (`devenv-env-global-mode`, lisp/devenv.el)
        under launchd/systemd daemons whose login shell does not export
        it. Opt-in because exec-path-from-shell normally finds the
        user's own devenv, and `pkgs.devenv` bundles its own nix and
        can version-skew against per-project devenv installs
      '';
    };

    spell = {
      dictionaries = lib.mkOption {
        type = with lib.types; listOf package;
        default = [ pkgs.aspellDicts.en ];
        defaultText = lib.literalExpression "[ pkgs.aspellDicts.en ]";
        example = lib.literalExpression "[ pkgs.aspellDicts.en pkgs.aspellDicts.fi ]";
        description = ''
          Aspell dictionary packages for jinx spell-checking
          (lisp/init-writing.el). Installed into the system profile, where
          libaspell's NIX_PROFILES patch finds them at runtime and
          enchant's aspell backend hands them to jinx.
        '';
      };
    };

    dockerfileLsp = {
      enable = lib.mkEnableOption "Dockerfile language server ({command}`docker-langserver`), auto-attached by Eglot in {command}`dockerfile-mode`";
    };

    onePassword = {
      enable = lib.mkEnableOption ''
        the 1Password CLI ({command}`op`) on the wrapper PATH, the backend
        for {command}`auth-source-1password` (lisp/init-systems.el) that
        resolves credentials for gptel, {command}`forge`, smtpmail, etc.
        from the vault. Pulls the unfree `_1password-cli` package, so it
        needs `allowUnfree`
      '';
    };

    sops = {
      enable = lib.mkEnableOption ''
        the {command}`sops` CLI on the wrapper PATH, required by
        {command}`sops.el` (lisp/init-systems.el) for transparent
        encrypt/decrypt of SOPS-managed files
      '';
    };

    claudeCode = {
      enable = lib.mkEnableOption ''
        the Claude Code CLI ({command}`claude`) on the wrapper PATH, the
        external agent {command}`claude-code-ide` (lisp/init-ai.el) drives.
        Pulls the unfree `claude-code` package, so it needs `allowUnfree`
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ jotainOverlay ];
    environment.systemPackages = [
      wrappedPackage
      editorScript
      visualScript
      pkgsWithOverlay.eca
    ]
    # jinx dictionaries: libaspell finds $profile/lib/aspell via its
    # NIX_PROFILES patch, so they belong in the profile, not on PATH.
    ++ cfg.spell.dictionaries;
    # Colour-emoji fallback for lisp/init-ui.el's fontsets. Skipped on
    # Darwin: macOS ships Apple Color Emoji, and nix-darwin's
    # `fonts.packages' differs from NixOS's.
    fonts.packages = lib.mkIf pkgs.stdenv.hostPlatform.isLinux [
      pkgs.noto-fonts-color-emoji
    ];
    environment.variables = lib.mkIf cfg.defaultEditor {
      EDITOR = lib.mkOverride 900 "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = lib.mkOverride 900 "${lib.getBin visualScript}/bin/jotain-visual";
    };
  };
}

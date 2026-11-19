# Options shared by module.nix (Home Manager), module-system.nix and its
# per-user submodule.
{ lib }:
{
  package = lib.mkOption {
    type = lib.types.nullOr lib.types.package;
    default = null;
    defaultText = lib.literalExpression "null";
    description = ''
      Custom Jotain Emacs package to use. Leave unset for the full
      distribution (`jotainEmacsPackages`, the pgtk/Wayland GUI).
    '';
  };

  nativeCompile.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    example = true;
    description = ''
      AOT native-compile the Jotain config into the store, so Emacs loads
      `.eln` for `init.el` and `lisp/` instead of JIT-compiling into
      `var/eln-cache` after every deploy that touches `lisp/` (which moves
      the store path and invalidates that cache). `early-init.el` always runs
      from `.elc`: its `.eln` lookup happens before it extends
      `native-comp-eln-load-path`.

      Emacs `realpath()`s the source before hashing it into the `.eln` name
      (src/comp.c, Bug#44701), so the Home Manager symlinks resolve to the
      path the AOT step compiled. Off by default for the cost: a
      native-compilation pass whenever the config rebuilds, and roughly
      50-150 MB of `.eln`.
    '';
  };

  defaultEditor = lib.mkOption {
    type = lib.types.bool;
    default = true;
    example = false;
    description = ''
      Whether to set {env}`EDITOR` and {env}`VISUAL` to the
      {command}`jotain-editor` and {command}`jotain-visual` scripts:
      {command}`emacsclient` with a fallback to a standalone Emacs when no
      daemon runs. Use this instead of `services.emacs.defaultEditor`.
    '';
  };

  sonarlint.enable = lib.mkEnableOption "SonarLint language server ({command}`M-x jotain-sonarlint`) on the wrapper PATH";

  dockerfileLsp.enable = lib.mkEnableOption "Dockerfile language server ({command}`docker-langserver`), auto-attached by Eglot in {command}`dockerfile-mode`, on the wrapper PATH";

  onePassword.enable = lib.mkEnableOption ''
    the 1Password CLI ({command}`op`) on the wrapper PATH, the backend for
    {command}`auth-source-1password` (lisp/init-systems.el) that resolves
    credentials for gptel, {command}`forge`, smtpmail, etc. from the vault.
    Pulls the unfree `_1password-cli` package, so it needs `allowUnfree`
  '';

  sops.enable = lib.mkEnableOption ''
    the {command}`sops` CLI on the wrapper PATH, required by
    {command}`sops.el` (lisp/init-systems.el) for transparent encrypt/decrypt
    of SOPS-managed files
  '';

  claudeCode.enable = lib.mkEnableOption ''
    the Claude Code CLI ({command}`claude`) on the wrapper PATH, the external
    agent {command}`claude-code-ide` (lisp/init-ai.el, `C-c q`) drives. Pulls
    the unfree `claude-code` package, so it needs `allowUnfree`
  '';

  spell.dictionaries = lib.mkOption {
    type = with lib.types; listOf package;
    default = [ ];
    example = lib.literalExpression "[ pkgs.aspellDicts.en pkgs.aspellDicts.fi ]";
    description = ''
      Aspell dictionary packages for jinx spell-checking
      (lisp/init-writing.el). A non-empty list replaces the bundled
      en/fi/de/fr set: the wrapped emacs gets an ASPELL_CONF that points at
      an {command}`aspellWithDicts` built from these packages. The packages
      also go into the profile. The empty default keeps the bundled
      dictionaries.
    '';
  };
}

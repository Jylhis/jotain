# Quickstart: Jotain Baseline

## With NixOS + Home Manager

```nix
# flake.nix inputs
inputs.jotain.url = "github:Jylhis/jotain";

# home-manager configuration
{ inputs, ... }: {
  imports = [ inputs.jotain.homeModules.default ];
  programs.jotain.enable = true;
}
```

Then: `home-manager switch`

**Verify:** Launch `emacs` — should start with no errors, no network package downloads.

## Without Nix

```bash
git clone https://github.com/Jylhis/jotain.git ~/.config/emacs
emacs
```

On first launch, packages will be downloaded from MELPA/GNU ELPA automatically.

**Verify:** `M-x describe-variable RET jotain/nix-managed-p RET` — should show `nil`.

## Validation Checklist

- [ ] Emacs starts without errors
- [ ] `jotain/nix-managed-p` reflects the environment correctly
- [ ] `jotain/platform` shows the correct platform identifier
- [ ] `private.el` is loaded if present (create one with `(message "private loaded")` to verify)
- [ ] On Nix: no `package-archives` configured
- [ ] On non-Nix: MELPA + GNU ELPA configured, packages auto-installed

## Running Tests

```bash
# Direct (fastest, requires dev shell)
just test-tag smoke     # < 1 second
just test-tag fast      # < 5 seconds

# Nix-sandboxed
just test-smoke
just test-fast
just test               # Full suite
```

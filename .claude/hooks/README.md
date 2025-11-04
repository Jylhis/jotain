# Claude Code Hooks

This directory contains hook scripts for automating session setup and development workflows.

## SessionStart Hook

The `session_setup.sh` script runs automatically when Claude Code starts a new session. It ensures the development environment is properly configured.

### What it does:

1. **Detects Environment**
   - Identifies cloud/ephemeral environments (containers, Codespaces, Gitpod, etc.)
   - Assumes Nix is pre-installed in local environments

2. **Installs Nix** (cloud environments only)
   - Uses the Determinate Systems installer for reliability
   - Single-user installation optimized for ephemeral environments
   - Skipped on local machines (assumes pre-existing installation)

3. **Configures Nix**
   - Enables experimental features: `nix-command` and `flakes`
   - Sets up `~/.config/nix/nix.conf` with optimal settings
   - Idempotent: safe to run multiple times

4. **Sources Nix Environment**
   - Ensures Nix commands are available in PATH
   - Works with both single-user and multi-user installations
   - Shows helpful error if Nix is missing locally

5. **Displays Project Information**
   - Shows available `just` commands
   - Lists common development workflows
   - Provides helpful tips for getting started

### Configuration

The hook is configured in `.claude/settings.json`:

```json
{
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "/home/user/jotain/.claude/hooks/session_setup.sh"
          }
        ]
      }
    ]
  }
}
```

### Usage

The hook runs automatically on session start. No manual intervention needed!

After the hook completes, you can:
- Run `nix develop` to enter the development shell
- Use any `just` command for development tasks
- Start coding with all dependencies available

### Customization

To modify the setup behavior, edit `session_setup.sh`:

**Auto-enter dev shell:**
Uncomment the line at the end of the script:
```bash
exec nix develop
```

**Add additional setup steps:**
Add your commands before the "Session setup complete" message.

**Install additional tools:**
Add package installations or tool setup after Nix is configured.

## Testing the Hook

To test the hook manually:
```bash
./.claude/hooks/session_setup.sh
```

## Troubleshooting

**Nix not in PATH:**
The script attempts to add Nix to PATH automatically. If it still doesn't work, manually source:
```bash
source ~/.nix-profile/etc/profile.d/nix.sh
```

**Permission denied:**
Ensure the script is executable:
```bash
chmod +x .claude/hooks/session_setup.sh
```

**Flakes not enabled:**
The script creates `~/.config/nix/nix.conf` automatically. Check its contents:
```bash
cat ~/.config/nix/nix.conf
```

## Resources

- [Claude Code Hooks Documentation](https://docs.claude.com/en/docs/claude-code/hooks)
- [Determinate Systems Nix Installer](https://github.com/DeterminateSystems/nix-installer)
- [Nix Flakes Guide](https://nixos.wiki/wiki/Flakes)

# Shell Script Conventions

This document describes the shell scripting conventions used in the Jotain project.

## File Extensions

### `.bash` - Bash Scripts
- **Shebang**: `#!/usr/bin/env bash`
- **Purpose**: Scripts that use Bash-specific features
- **Features allowed**:
  - Arrays: `arr=(one two three)`
  - Extended test operators: `[[ ]]`
  - Process substitution: `<(command)`
  - Here-strings: `<<< "string"`
  - `${BASH_SOURCE[0]}` and other Bash variables
  - `local` keyword for function-scoped variables
  - Extended globbing: `shopt -s extglob`

### `.sh` - POSIX Shell Scripts
- **Shebang**: `#!/bin/sh`
- **Purpose**: Portable scripts that work on any POSIX-compliant shell
- **Restrictions**:
  - No Bash-specific features
  - Use `[ ]` instead of `[[ ]]`
  - Use portable variable expansion
  - Compatible with dash, ash, busybox sh, etc.

## Current Project Files

All current CLI scripts use Bash features and are named with `.bash` extension:
- `cli/jot` - Main CLI entry point (Bash, no extension as it's executable)
- `cli/lib/common.bash` - Common utilities (uses Bash arrays)
- `cli/lib/emacs.bash` - Emacs invocation utilities (uses Bash features)
- `cli/commands/*.bash` - All command implementations (use Bash features)

## Linting and Formatting

### ShellCheck
ShellCheck is configured via `.shellcheckrc`:
- `.bash` files: Checked as Bash (dialect=bash inferred from shebang)
- `.sh` files: Checked as POSIX sh (dialect=sh)

### shfmt
Shell formatter configured via treefmt:
- Formats both `.bash` and `.sh` files
- 2-space indentation
- Simplifies code where possible
- Run with: `just fmt` or `nix fmt`

## Best Practices

1. **Choose the right extension**:
   - Use `.bash` if you need Bash features
   - Use `.sh` only if script must be portable to non-Bash shells

2. **Always use strict mode** in Bash scripts:
   ```bash
   #!/usr/bin/env bash
   set -euo pipefail
   ```

3. **Quote variables** to prevent word splitting:
   ```bash
   # Good
   echo "$var"

   # Bad
   echo $var
   ```

4. **Use `local` for function variables** in Bash:
   ```bash
   my_function() {
       local temp="value"
       echo "$temp"
   }
   ```

5. **Source other scripts** with proper path resolution:
   ```bash
   # Good (works from any directory)
   SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
   source "$SCRIPT_DIR/lib/common.bash"

   # Avoid (breaks if run from different directory)
   source ./lib/common.bash
   ```

## Examples

### Bash Script Example (`cli/lib/common.bash`)
```bash
#!/usr/bin/env bash
# Common utilities for Jotain CLI

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Logging with colors (uses Bash string interpolation)
info() {
    echo -e "${GREEN}==>${NC} $*"
}

# Check if command exists (uses 'command' builtin)
has_command() {
    command -v "$1" >/dev/null 2>&1
}
```

### POSIX sh Script Example (future use)
```sh
#!/bin/sh
# Portable installation script

# No colors in POSIX sh (or use tput for portability)
info() {
    printf '==> %s\n' "$*"
}

# POSIX-compliant command check
has_command() {
    command -v "$1" >/dev/null 2>&1
}
```

## Migration Checklist

When converting a Bash script to POSIX sh:
- [ ] Change shebang to `#!/bin/sh`
- [ ] Rename from `.bash` to `.sh`
- [ ] Replace `[[ ]]` with `[ ]`
- [ ] Replace arrays with space-separated strings or multiple variables
- [ ] Replace `${BASH_SOURCE[0]}` with `$0` (may need adjustment)
- [ ] Remove `local` keyword (not in POSIX sh)
- [ ] Test on multiple shells (dash, ash, busybox)
- [ ] Run `shellcheck` to verify POSIX compliance

## References

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [POSIX Shell Command Language](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html)
- [ShellCheck Wiki](https://github.com/koalaman/shellcheck/wiki)
- [Google Shell Style Guide](https://google.github.io/styleguide/shellguide.html)

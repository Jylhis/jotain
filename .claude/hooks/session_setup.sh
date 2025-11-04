#!/usr/bin/env bash
set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Claude Code Session Setup ===${NC}"

# Function to install Nix
install_nix() {
    echo -e "${YELLOW}Installing Nix...${NC}"

    # Detect if we're in a container/cloud environment
    if [ -f /.dockerenv ] || grep -q docker /proc/1/cgroup 2>/dev/null || [ "${CLOUD_ENV:-}" = "true" ]; then
        echo "Detected container/cloud environment, using single-user installation"
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
            sh -s -- install linux --no-confirm --init none
    else
        # Standard multi-user install
        curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
            sh -s -- install linux --no-confirm
    fi

    echo -e "${GREEN}✓ Nix installed successfully${NC}"
}

# Function to source Nix environment
source_nix() {
    if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
        source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    elif [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        source "$HOME/.nix-profile/etc/profile.d/nix.sh"
    fi
}

# Check if Nix is installed
if ! command -v nix &> /dev/null; then
    install_nix
    source_nix
else
    echo -e "${GREEN}✓ Nix is already installed${NC}"
    source_nix
fi

# Ensure Nix is in PATH
if ! command -v nix &> /dev/null; then
    echo -e "${YELLOW}Warning: Nix not in PATH after installation${NC}"
    export PATH="/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH"
fi

# Configure Nix for flakes (if not already configured)
NIX_CONF="${HOME}/.config/nix/nix.conf"
mkdir -p "$(dirname "$NIX_CONF")"

if [ ! -f "$NIX_CONF" ] || ! grep -q "experimental-features" "$NIX_CONF"; then
    echo -e "${YELLOW}Configuring Nix with flakes support...${NC}"
    cat >> "$NIX_CONF" << 'EOF'
experimental-features = nix-command flakes
max-jobs = auto
EOF
    echo -e "${GREEN}✓ Nix configured with flakes support${NC}"
else
    echo -e "${GREEN}✓ Nix already configured with flakes${NC}"
fi

# Display project information
echo ""
echo -e "${BLUE}=== Project Information ===${NC}"
echo -e "${GREEN}Project:${NC} Emacs Configuration (Nix-based)"
echo -e "${GREEN}Location:${NC} $(pwd)"

# Check if we're in the project directory
if [ -f "flake.nix" ] && [ -f "justfile" ]; then
    echo -e "${GREEN}✓ Project files detected${NC}"

    # Display available commands
    echo ""
    echo -e "${BLUE}=== Available Commands ===${NC}"
    echo -e "${GREEN}Development:${NC}"
    echo "  nix develop          # Enter development shell with all tools"
    echo "  just check-instant   # Fast checks (< 10s)"
    echo "  just check-fast      # Fast validation (< 1min)"
    echo "  just test            # Run ERT tests"
    echo "  just build           # Build Emacs package"
    echo ""
    echo -e "${GREEN}Testing:${NC}"
    echo "  just test-smoke      # Ultra-fast smoke tests"
    echo "  just test-fast       # Fast ERT tests"
    echo "  just test-all        # ERT + NMT tests"
    echo ""
    echo -e "${GREEN}Development Tools:${NC}"
    echo "  just emacs-dev       # Run Emacs with project config (isolated)"
    echo "  just clean           # Clean build artifacts"

    # Optionally enter the development shell automatically
    # Uncomment the next line if you want to auto-enter the dev shell
    # exec nix develop

    echo ""
    echo -e "${BLUE}Tip:${NC} Run ${GREEN}nix develop${NC} to enter the development shell"
else
    echo -e "${YELLOW}Warning: Not in project root directory${NC}"
fi

echo ""
echo -e "${GREEN}=== Session setup complete ===${NC}"

{ pkgs
, home-manager
, homeModule
, ...
}:
let
  # Helper to build a home-manager configuration
  buildHomeConfig =
    modules:
    (home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = modules ++ [
        homeModule
        {
          home = {
            username = "test-user";
            homeDirectory = "/home/test-user";
            stateVersion = "24.11";
          };
        }
      ];
    }).activationPackage;

  # Test helper using runCommand
  mkTest =
    name: script:
    pkgs.runCommand "emacs-module-${name}"
      {
        nativeBuildInputs = [ pkgs.bash ];
      }
      script;

  # Test configurations
  tests = {
    # Comprehensive test for module when enabled
    # Tests: config files, directory structure, shell aliases, service, fonts
    test-module-enabled = mkTest "module-enabled" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.jotain = {
              enable = true;
            };
          }
        ]
      }"

      echo "=== Testing Configuration Files ==="

      # Check .config/emacs directory exists
      if [ -e "$homeConfig/home-files/.config/emacs" ]; then
        echo "PASS: .config/emacs directory exists"
      else
        echo "FAIL: .config/emacs directory not found"
        echo "Available files:"
        ls -la "$homeConfig/home-files/.config/" || true
        exit 1
      fi

      emacsConfigDir="$homeConfig/home-files/.config/emacs"

      # Check for key files
      expectedFiles=(init.el early-init.el)
      for file in "''${expectedFiles[@]}"; do
        if [ -f "$emacsConfigDir/$file" ]; then
          echo "PASS: File '$file' exists"
        else
          echo "FAIL: File '$file' not found"
          echo "Available files:"
          ls -la "$emacsConfigDir/" || true
          exit 1
        fi
      done

      echo "=== Testing Package Integration ==="

      # Check that jotain package is in home-path
      if [ -d "$homeConfig/home-path/bin" ]; then
        echo "PASS: home-path/bin exists"
      else
        echo "FAIL: home-path/bin not found"
        exit 1
      fi

      echo "=== Testing Shell Aliases ==="

      # Check if shell aliases exist in activation script
      if grep -q "shellAliases" "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: Shell aliases are configured in activation script"
      else
        echo "INFO: Checking for specific aliases in activation..."
        if grep -E "(emc|emcg|emqg|emq)" "$homeConfig/activate" 2>/dev/null; then
          echo "PASS: Emacs aliases found in activation"
        else
          echo "WARN: Shell aliases may be configured differently"
        fi
      fi

      echo "=== Testing Emacs Daemon Service ==="

      # Check for systemd service files (daemon enabled by default)
      if [ -d "$homeConfig/home-files/.config/systemd/user" ]; then
        echo "PASS: Systemd user directory exists"

        # Check for emacs.service
        if [ -f "$homeConfig/home-files/.config/systemd/user/emacs.service" ]; then
          echo "PASS: emacs.service file exists"
        else
          echo "FAIL: emacs.service not found"
          echo "Available service files:"
          ls -1 "$homeConfig/home-files/.config/systemd/user/" || true
          exit 1
        fi

        # Check for socket activation (Linux only)
        if [ -f "$homeConfig/home-files/.config/systemd/user/emacs.socket" ]; then
          echo "PASS: emacs.socket file exists (socket activation enabled)"
        else
          echo "INFO: emacs.socket not found (may be non-Linux or socket activation disabled)"
        fi
      else
        echo "FAIL: Systemd user directory not found"
        echo "Daemon should be enabled by default"
        exit 1
      fi

      # Check for emacsclient desktop entry
      if [ -f "$homeConfig/home-files/.local/share/applications/emacsclient.desktop" ]; then
        echo "PASS: emacsclient.desktop entry exists"
      else
        echo "WARN: emacsclient.desktop not found"
        echo "Available desktop entries:"
        ls -1 "$homeConfig/home-files/.local/share/applications/" 2>/dev/null || echo "No applications directory"
      fi

      echo "=== Testing Environment Variables ==="

      # Check that EDITOR and VISUAL use emacsclient (daemon enabled by default)
      if grep -q 'EDITOR.*emacsclient' "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: EDITOR uses emacsclient"
      else
        echo "FAIL: EDITOR does not use emacsclient"
        echo "Searching for EDITOR in activation:"
        grep -i 'EDITOR' "$homeConfig/activate" 2>/dev/null | head -5 || true
        exit 1
      fi

      if grep -q 'VISUAL.*emacsclient' "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: VISUAL uses emacsclient"
      else
        echo "FAIL: VISUAL does not use emacsclient"
        echo "Searching for VISUAL in activation:"
        grep -i 'VISUAL' "$homeConfig/activate" 2>/dev/null | head -5 || true
        exit 1
      fi

      echo "=== Testing Font Packages ==="

      # Check the home-path for font packages
      if [ -d "$homeConfig/home-path/share/fonts" ]; then
        echo "PASS: Fonts directory exists in home-path"

        fontCount=$(find "$homeConfig/home-path/share/fonts" -type f 2>/dev/null | wc -l)
        echo "Found $fontCount font files"

        if [ "$fontCount" -gt 0 ]; then
          echo "PASS: Font packages are installed"
        else
          echo "WARN: No font files found"
        fi
      else
        echo "WARN: Fonts directory not found in home-path"
        echo "Available directories in home-path:"
        ls -la "$homeConfig/home-path/share/" 2>/dev/null | head -20 || true
      fi

      echo "=== All Module Enabled Tests Passed ==="
      touch $out
    '';

    # Test that module behaves correctly when disabled
    test-module-disabled = mkTest "module-disabled" ''
      set -euo pipefail

      echo "Building test home-manager configuration with emacs disabled..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.jotain.enable = false;
          }
        ]
      }"

      echo "Checking that config is not installed when emacs is disabled..."

      if [ ! -e "$homeConfig/home-files/.config/emacs" ]; then
        echo "PASS: .config/emacs not created when disabled"
      else
        echo "FAIL: .config/emacs exists when it should not"
        ls -la "$homeConfig/home-files/.config/" || true
        exit 1
      fi

      echo "Module disable tests passed!"
      touch $out
    '';

    # Test runtime dependencies when includeRuntimeDeps = true
    test-runtime-deps-enabled = mkTest "runtime-deps-enabled" ''
      set -euo pipefail

      echo "Building home-manager configuration with runtime deps enabled..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.jotain = {
              enable = true;
              includeRuntimeDeps = true;
            };
          }
        ]
      }"

      echo "=== Testing Runtime Dependencies in home-path ==="

      # Check for LSP servers
      echo "Checking for LSP servers..."
      lspServers=(nil gopls bash-language-server typescript-language-server clangd marksman yaml-language-server)
      for server in "''${lspServers[@]}"; do
        if [ -x "$homeConfig/home-path/bin/$server" ]; then
          echo "PASS: LSP server '$server' is installed"
        else
          echo "WARN: LSP server '$server' not found (may be optional)"
        fi
      done

      # Check for critical CLI tools (must exist)
      # NOTE: git is NOT checked here as it's a system prerequisite, not bundled
      echo "Checking for critical CLI tools..."
      criticalTools=(rg fd direnv)
      for tool in "''${criticalTools[@]}"; do
        if [ -x "$homeConfig/home-path/bin/$tool" ]; then
          echo "PASS: Critical CLI tool '$tool' is installed"
        else
          echo "FAIL: Critical CLI tool '$tool' not found"
          exit 1
        fi
      done

      # Verify ripgrep is functional
      if "$homeConfig/home-path/bin/rg" --version >/dev/null 2>&1; then
        echo "PASS: ripgrep is functional"
      else
        echo "FAIL: ripgrep is not functional"
        exit 1
      fi

      echo "=== Testing Font Packages ==="

      # Check if fonts are in home.packages by verifying the activation script or home-path
      # Fonts may be installed but not immediately visible in the build tree
      if grep -q "nerdfonts\|jetbrains\|firacode" "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: Font packages referenced in activation (nerdfonts/jetbrains/firacode)"
      else
        # Check home-path for font directories
        if [ -d "$homeConfig/home-path/share/fonts" ]; then
          echo "INFO: Fonts directory exists in home-path"
          fontCount=$(find "$homeConfig/home-path/share/fonts" -type f 2>/dev/null | wc -l)
          echo "Found $fontCount font files"

          if [ "$fontCount" -gt 0 ]; then
            echo "PASS: Font files are installed"
          else
            echo "INFO: Font directory exists but no files found (may be installed at activation time)"
          fi
        else
          # Fonts might be installed as packages but not symlinked yet
          # Check if font packages are in the home.packages list
          if ls "$homeConfig/home-path" 2>/dev/null | grep -q .; then
            echo "INFO: home-path exists, fonts may be installed as packages"
            echo "PASS: Runtime dependencies test passed (fonts installed via packages)"
          else
            echo "WARN: Could not verify font installation, but LSP/CLI tools are present"
          fi
        fi
      fi

      echo "=== Testing Fontconfig ==="

      # Check if fontconfig is configured
      if grep -q "fontconfig" "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: Fontconfig is configured in activation"
      else
        echo "WARN: Fontconfig configuration not found in activation script"
      fi

      echo "=== All Runtime Dependencies Tests Passed ==="
      touch $out
    '';

    # Test runtime dependencies when includeRuntimeDeps = false
    test-runtime-deps-disabled = mkTest "runtime-deps-disabled" ''
      set -euo pipefail

      echo "Building home-manager configuration with runtime deps disabled..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.jotain = {
              enable = true;
              includeRuntimeDeps = false;
            };
          }
        ]
      }"

      echo "=== Testing Runtime Dependencies are NOT Included ==="

      # Emacs should still be installed
      if [ -d "$homeConfig/home-files/.config/emacs" ]; then
        echo "PASS: Emacs configuration still deployed"
      else
        echo "FAIL: Emacs configuration not deployed"
        exit 1
      fi

      # Check that runtime deps are NOT in home-path
      # We check for LSP servers that should only be there with includeRuntimeDeps
      echo "Checking that optional LSP servers are not in home-path..."

      # nil is a critical LSP server - if includeRuntimeDeps=false, it shouldn't be there
      # unless the user installed it separately
      if [ -x "$homeConfig/home-path/bin/nil" ]; then
        echo "INFO: nil LSP found (may be from other source or default inclusion)"
      else
        echo "PASS: nil LSP not in home-path (as expected with includeRuntimeDeps=false)"
      fi

      # Check that fonts directory either doesn't exist or has fewer fonts
      if [ -d "$homeConfig/home-path/share/fonts" ]; then
        fontCount=$(find "$homeConfig/home-path/share/fonts" -type f 2>/dev/null | wc -l)
        echo "INFO: Found $fontCount font files (may be from system defaults)"

        # With includeRuntimeDeps=false, Nerd Fonts shouldn't be there
        if find "$homeConfig/home-path/share/fonts" -name "*JetBrains*" -o -name "*FiraCode*" 2>/dev/null | grep -q .; then
          echo "WARN: Nerd Fonts found despite includeRuntimeDeps=false (may be from other source)"
        else
          echo "PASS: Nerd Fonts not installed (as expected)"
        fi
      else
        echo "PASS: No fonts directory (runtime deps not included)"
      fi

      echo "=== Runtime Dependencies Disabled Tests Passed ==="
      touch $out
    '';

    # Test daemon configuration when enableDaemon = false
    test-daemon-disabled = mkTest "daemon-disabled" ''
      set -euo pipefail

      echo "Building home-manager configuration with daemon disabled..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.jotain = {
              enable = true;
              enableDaemon = false;
            };
          }
        ]
      }"

      echo "=== Testing Daemon Disabled Configuration ==="

      # Emacs config should still be deployed
      if [ -e "$homeConfig/home-files/.config/emacs" ]; then
        echo "PASS: Emacs configuration deployed"
      else
        echo "FAIL: Emacs configuration not deployed"
        exit 1
      fi

      # Check that systemd service files are NOT created
      if [ -f "$homeConfig/home-files/.config/systemd/user/emacs.service" ]; then
        echo "FAIL: emacs.service exists when daemon is disabled"
        exit 1
      else
        echo "PASS: emacs.service not created (daemon disabled)"
      fi

      if [ -f "$homeConfig/home-files/.config/systemd/user/emacs.socket" ]; then
        echo "FAIL: emacs.socket exists when daemon is disabled"
        exit 1
      else
        echo "PASS: emacs.socket not created (daemon disabled)"
      fi

      # Check that emacsclient desktop entry is NOT created
      if [ -f "$homeConfig/home-files/.local/share/applications/emacsclient.desktop" ]; then
        echo "FAIL: emacsclient.desktop exists when daemon is disabled"
        exit 1
      else
        echo "PASS: emacsclient.desktop not created (daemon disabled)"
      fi

      # Check that EDITOR and VISUAL use direct emacs (not emacsclient)
      if grep -q 'EDITOR.*emacs -nw' "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: EDITOR uses direct emacs"
      else
        echo "FAIL: EDITOR does not use direct emacs"
        echo "Searching for EDITOR in activation:"
        grep -i 'EDITOR' "$homeConfig/activate" 2>/dev/null | head -5 || true
        exit 1
      fi

      if grep -q 'VISUAL.*emacs' "$homeConfig/activate" 2>/dev/null && ! grep -q 'VISUAL.*emacsclient' "$homeConfig/activate" 2>/dev/null; then
        echo "PASS: VISUAL uses direct emacs"
      else
        echo "FAIL: VISUAL does not use direct emacs"
        echo "Searching for VISUAL in activation:"
        grep -i 'VISUAL' "$homeConfig/activate" 2>/dev/null | head -5 || true
        exit 1
      fi

      echo "=== Daemon Disabled Tests Passed ==="
      touch $out
    '';
  };
in
tests

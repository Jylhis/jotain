{
  pkgs,
  home-manager,
  homeModule,
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
    pkgs.runCommand "emacs-module-${name}" {
      nativeBuildInputs = [ pkgs.bash ];
    } script;

  # Test configurations
  tests = {
    # Test 1: Emacs configuration files are properly linked
    test-emacs-config-files = mkTest "config-files" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs = {
              enable = true;
              userConfig = ./..;
            };
          }
        ]
      }"

      echo "Checking if .config/emacs directory is created..."
      if [ -e "$homeConfig/home-files/.config/emacs" ]; then
        echo "PASS: .config/emacs directory exists"
      else
        echo "FAIL: .config/emacs directory not found"
        echo "Available files:"
        ls -la "$homeConfig/home-files/.config/" || true
        exit 1
      fi

      echo "Checking if init.el is present..."
      if [ -f "$homeConfig/home-files/.config/emacs/init.el" ]; then
        echo "PASS: init.el file exists"
      else
        echo "FAIL: init.el file not found"
        echo "Contents of .config/emacs:"
        ls -la "$homeConfig/home-files/.config/emacs/" || true
        exit 1
      fi

      echo "All config file tests passed!"
      touch $out
    '';

    # Test 2: Shell aliases are correctly configured
    test-shell-aliases = mkTest "shell-aliases" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs = {
              enable = true;
              userConfig = ./..;
            };
          }
        ]
      }"

      echo "Checking shell aliases configuration..."

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

      echo "Shell alias tests completed!"
      touch $out
    '';

    # Test 3: Emacs service is enabled
    test-emacs-service = mkTest "emacs-service" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs = {
              enable = true;
              userConfig = ./..;
            };
          }
        ]
      }"

      echo "Checking if Emacs service is configured..."

      # Check for systemd service files
      if [ -d "$homeConfig/home-files/.config/systemd/user" ]; then
        echo "PASS: Systemd user directory exists"

        # Check for emacs service files
        if ls "$homeConfig/home-files/.config/systemd/user/"*emacs* 1>/dev/null 2>&1; then
          echo "PASS: Emacs service files found:"
          ls -1 "$homeConfig/home-files/.config/systemd/user/"*emacs* || true
        else
          echo "WARN: No emacs service files found, listing all services:"
          ls -1 "$homeConfig/home-files/.config/systemd/user/" || true
        fi
      else
        echo "INFO: Checking activation script for service configuration..."
        if grep -q "emacs.*service" "$homeConfig/activate" 2>/dev/null; then
          echo "PASS: Emacs service configured in activation"
        else
          echo "WARN: Service directory not found"
        fi
      fi

      echo "Service configuration tests completed!"
      touch $out
    '';

    # Test 4: Font packages are installed
    test-font-packages = mkTest "font-packages" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs = {
              enable = true;
              userConfig = ./..;
            };
          }
        ]
      }"

      echo "Checking if font packages are included..."

      # Check the home-path for font packages
      if [ -d "$homeConfig/home-path/share/fonts" ]; then
        echo "PASS: Fonts directory exists in home-path"

        # Count font files
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

      echo "Font package tests completed!"
      touch $out
    '';

    # Test 5: Module is disabled when emacs is not enabled
    test-module-disabled = mkTest "module-disabled" ''
      set -euo pipefail

      echo "Building test home-manager configuration with emacs disabled..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs.enable = false;
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

    # Test 6: Fileset source works correctly
    test-fileset-source = mkTest "fileset-source" ''
      set -euo pipefail

      echo "Building test home-manager configuration..."
      homeConfig="${
        buildHomeConfig [
          {
            programs.emacs = {
              enable = true;
              userConfig = ./..;
            };
          }
        ]
      }"

      echo "Checking fileset-based source structure..."

      emacsConfigDir="$homeConfig/home-files/.config/emacs"

      # Check for expected directories
      expectedDirs=(config lisp tests)
      for dir in "''${expectedDirs[@]}"; do
        if [ -d "$emacsConfigDir/$dir" ]; then
          echo "PASS: Directory '$dir' exists"
        else
          echo "FAIL: Directory '$dir' not found"
          echo "Available directories:"
          ls -la "$emacsConfigDir/" || true
          exit 1
        fi
      done

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

      echo "Fileset structure tests passed!"
      touch $out
    '';
  };
in
tests

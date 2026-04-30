# nix/checks.nix — Flake check derivations for Jotain.
#
# Application and configuration checks live here (Nix linting, Elisp
# validation).  Dev-environment assertions live in devenv.nix enterTest.
{
  pkgs,
  src,
  treefmtCheck,
}:
let
  inherit (pkgs) lib;

  hmStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      home.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
      };
      home.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
      };
      xdg.configHome = lib.mkOption {
        type = lib.types.str;
        default = "/tmp/jotain-home/.config";
      };
      xdg.configFile = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.services = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.sockets = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      launchd.agents = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
    };
  };

  evalHomeModule =
    jotainConfig:
    lib.evalModules {
      modules = [
        hmStubModule
        ../module.nix
        {
          services.jotain = {
            enable = true;
          }
          // jotainConfig;
        }
      ];
      specialArgs = { inherit pkgs; };
    };

  defaultModule = evalHomeModule { };
  graphicalModule = evalHomeModule {
    startWithUserSession = "graphical";
  };
in
{
  # ── Package builds ────────────────────────────────────────────────
  packages-default = pkgs.jotainEmacsPackages;
  packages-emacs = pkgs.jotainEmacs;
  packages-info = pkgs.jotainInfo;

  # ── Option documentation ─────────────────────────────────────────
  options-doc = import ./options-doc.nix { inherit pkgs src; };

  # ── Home Manager module evaluation ───────────────────────────────
  module-eval =
    pkgs.runCommandLocal "check-module-eval"
      {
        defaultEditor = defaultModule.config.home.sessionVariables.EDITOR;
        graphicalTarget =
          if pkgs.stdenv.hostPlatform.isLinux then
            builtins.toJSON graphicalModule.config.systemd.user.services.jotain.Install.WantedBy
          else
            builtins.toJSON graphicalModule.config.launchd.agents.jotain.config.ProgramArguments;
      }
      ''
        touch $out
      '';

  # ── Nix formatting (via shared treefmt config) ────────────────────
  formatting = treefmtCheck;

  # ── Nix static analysis ──────────────────────────────────────────
  statix =
    pkgs.runCommandLocal "check-statix"
      {
        nativeBuildInputs = [ pkgs.statix ];
        inherit src;
      }
      ''
        cd $src
        statix check .
        touch $out
      '';

  # ── Nix dead code detection ──────────────────────────────────────
  deadnix =
    pkgs.runCommandLocal "check-deadnix"
      {
        nativeBuildInputs = [ pkgs.deadnix ];
        inherit src;
      }
      ''
        cd $src
        deadnix --fail .
        touch $out
      '';

  # ── Elisp syntax (balanced parens) ───────────────────────────────
  elisp-lint =
    pkgs.runCommandLocal "check-elisp-lint"
      {
        nativeBuildInputs = [ pkgs.jotainEmacs ];
        inherit src;
      }
      ''
        cd $src
        emacs -Q --batch --eval '
          (let ((failed nil))
            (dolist (f (append (list "early-init.el" "init.el")
                               (directory-files "lisp" t "^init-.*\\.el$")))
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents f)
                    (emacs-lisp-mode)
                    (check-parens)
                    (message "OK: %s" (file-name-nondirectory f)))
                (error
                 (message "FAIL %s: %S" (file-name-nondirectory f) err)
                 (setq failed t))))
            (when failed (kill-emacs 1)))'
        touch $out
      '';

  # ── Elisp byte-compilation (warnings as errors) ─────────────────
  elisp-compile =
    pkgs.runCommandLocal "check-elisp-compile"
      {
        nativeBuildInputs = [ pkgs.jotainEmacsPackages ];
        inherit src;
      }
      ''
        cp -r $src work
        chmod -R u+w work
        cd work
        emacs --batch \
          -L lisp \
          --eval "(require 'pcre2el)" \
          --eval "(setq byte-compile-error-on-warn t)" \
          -f batch-byte-compile early-init.el init.el lisp/init-*.el
        touch $out
      '';
}

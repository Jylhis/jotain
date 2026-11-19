# Flake checks.
{
  pkgs,
  src,
  treefmtCheck,
  devShell,
}:
let
  inherit (pkgs) lib;
  inherit (lib) fileset;

  # `src` stays a parameter for options-doc.nix, whose `srcPrefix` strip
  # needs the flake source; lib.fileset needs a real path, so the narrowed
  # sources are built from ../.
  repoRoot = ../.;
  elIn = dir: fileset.fileFilter (f: lib.hasSuffix ".el" f.name) (repoRoot + dir);

  configFiles = fileset.unions [
    (repoRoot + "/early-init.el")
    (repoRoot + "/init.el")
    (elIn "/lisp")
  ];
  elispSrc = fileset.toSource {
    root = repoRoot;
    fileset = configFiles;
  };
  # test/lang-eval-test.el reads etc/lang-eval/ and templates/.
  elispTestSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.unions [
      configFiles
      (elIn "/test")
      (elIn "/etc/lang-eval")
      (repoRoot + "/templates")
    ];
  };
  nixSrc = fileset.toSource {
    root = repoRoot;
    fileset = fileset.fileFilter (f: lib.hasSuffix ".nix" f.name) repoRoot;
  };

  # The inner emacsWithPackages result, not the outer wrapper: the wrapper's
  # INFOPATH pulls in jotainInfo, so docs edits would invalidate the checks.
  elispEmacs = pkgs.jotainEmacsPackages.core;

  hmStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      home = {
        sessionVariables = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
        };
        username = lib.mkOption {
          type = lib.types.str;
          default = "tester";
        };
        packages = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [ ];
        };
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
      programs = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      fonts = lib.mkOption {
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
  spellModule = evalHomeModule { spell.dictionaries = [ pkgs.aspellDicts.fi ]; };
  emacsWrapperText =
    module:
    let
      scripts = lib.filter (
        p: lib.hasInfix "--init-directory" (p.text or "")
      ) module.config.home.packages;
    in
    if scripts == [ ] then "" else (lib.head scripts).text;
in
{
  packages-default = pkgs.jotainEmacsPackages;
  options-doc = import ./options-doc.nix { inherit pkgs src; };

  # config/eca/config.json and gptel's OpenRouter :models (lisp/init-ai.el)
  # are hand-kept copies of one list.
  eca-models-in-sync =
    pkgs.runCommand "check-eca-models-in-sync"
      {
        nativeBuildInputs = [
          pkgs.jq
          elispEmacs
        ];
        ecaConfig = repoRoot + "/config/eca/config.json";
        initAi = repoRoot + "/lisp/init-ai.el";
      }
      ''
        jsonModels=$(jq -r '.providers.openrouter.models | keys[]' "$ecaConfig" | sort)

        cat > extract.el <<'EOF'
        ;;; extract.el --- read gptel :models -*- lexical-binding: t; -*-
        (with-temp-buffer
          (insert-file-contents (getenv "INIT_AI"))
          (goto-char (point-min))
          (re-search-forward "gptel-make-openai \"OpenRouter\"")
          (re-search-forward ":models[[:space:]]*'")
          (dolist (m (read (current-buffer)))
            (princ (format "%s\n" m))))
        EOF
        elispModels=$(INIT_AI="$initAi" emacs -Q --batch -l extract.el | sort)

        if [ "$jsonModels" != "$elispModels" ]; then
          echo "" >&2
          echo "config/eca/config.json and lisp/init-ai.el disagree on the" >&2
          echo "OpenRouter model list (they are hand-kept copies of one" >&2
          echo "catalogue). Reconcile both. Diff (< json, > elisp):" >&2
          diff <(echo "$jsonModels") <(echo "$elispModels") >&2 || true
          exit 1
        fi
        touch $out
      '';

  module-eval =
    pkgs.runCommandLocal "check-module-eval"
      {
        defaultEditorConfigured = if defaultModule.config.home.sessionVariables ? EDITOR then "1" else "0";
        graphicalTarget = builtins.toJSON graphicalModule.config.systemd.user.services.jotain.Install.WantedBy;
        spellReachesWrapper =
          if lib.hasInfix "ASPELL_CONF='dict-dir " (emacsWrapperText spellModule) then "1" else "0";
        spellDefaultKeepsBundle =
          if lib.hasInfix "ASPELL_CONF" (emacsWrapperText defaultModule) then "0" else "1";
      }
      ''
        test "$defaultEditorConfigured" = "1" || { echo "EDITOR not set"; exit 1; }
        case "$graphicalTarget" in
          *graphical-session.target*) ;;
          *) echo "graphical start target not wired: $graphicalTarget"; exit 1 ;;
        esac
        test "$spellReachesWrapper" = "1" || { echo "spell.dictionaries does not reach ASPELL_CONF in the wrapper"; exit 1; }
        test "$spellDefaultKeepsBundle" = "1" || { echo "the default sets ASPELL_CONF in the wrapper"; exit 1; }
        touch $out
      '';

  dev-shell = devShell;

  formatting = treefmtCheck;

  statix =
    pkgs.runCommandLocal "check-statix"
      {
        nativeBuildInputs = [ pkgs.statix ];
        src = nixSrc;
      }
      ''
        cd $src
        statix check .
        touch $out
      '';

  deadnix =
    pkgs.runCommandLocal "check-deadnix"
      {
        nativeBuildInputs = [ pkgs.deadnix ];
        src = nixSrc;
      }
      ''
        cd $src
        deadnix --fail .
        touch $out
      '';

  elisp-compile = import ./config-compiled.nix {
    inherit pkgs;
    emacs = elispEmacs;
  };

  elisp-test =
    let
      ertJunit = (pkgs.emacsPackagesFor pkgs.jotainEmacs).ert-junit;
      report =
        pkgs.runCommand "check-elisp-test-report"
          {
            nativeBuildInputs = [
              elispEmacs
              pkgs.git
            ];
            src = elispTestSrc;
            inherit ertJunit;
          }
          ''
            mkdir -p $out
            cd $src
            status=0
            emacs --batch -L lisp -L test \
              -L "$(echo $ertJunit/share/emacs/site-lisp/elpa/ert-junit-*)" \
              --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
              -l ert -l ert-junit -f ert-junit-run-tests-batch-and-exit "$out/junit.xml" \
              >$out/output.log 2>&1 || status=$?
            cat $out/output.log
            echo $status > $out/exit-code
          '';
    in
    pkgs.runCommand "check-elisp-test"
      {
        inherit report;
        nativeBuildInputs = [ pkgs.gnugrep ];
        passthru = { inherit report; };
      }
      ''
        cat $report/output.log
        [ "$(cat $report/exit-code)" = 0 ] || { echo "ERT failed" >&2; exit 1; }
        if grep -lE '<(failure|error)[ />]' $report/*.xml; then
          echo "a JUnit file records a failure" >&2
          exit 1
        fi
        touch $out
      '';

  # The only gate that evaluates every use-package :config block. Run with
  # `use-package-always-ensure` off, so no :ensure triggers `package-install`
  # (no network in the sandbox). use-package demotes a failing :config to an
  # "Error (use-package)" warning, so a clean exit is not enough.
  config-startup =
    pkgs.runCommand "check-config-startup"
      {
        nativeBuildInputs = [ elispEmacs ];
        src = elispSrc;
      }
      ''
        set -euo pipefail
        workdir="$(mktemp -d)"
        cp -rL "$src"/. "$workdir"/
        chmod -R u+w "$workdir"
        export HOME="$workdir"

        emacs --batch \
          --eval "(setq user-emacs-directory (file-name-as-directory \"$workdir\"))" \
          -l "$workdir/early-init.el" \
          --eval '(setq use-package-always-ensure nil)' \
          -l "$workdir/init.el" \
          --eval '(message "jotain: config startup complete")' \
          > "$workdir/startup.log" 2>&1

        cat "$workdir/startup.log"

        if grep -qE "Error \(use-package\)|failed to define function" "$workdir/startup.log"; then
          echo "FAIL: a use-package :config block errored during startup" >&2
          echo "(commonly a stale ELPA package shadowing an Emacs built-in)" >&2
          exit 1
        fi
        if ! grep -qF "jotain: config startup complete" "$workdir/startup.log"; then
          echo "FAIL: startup aborted before loading the whole config" >&2
          exit 1
        fi
        touch $out
      '';
}

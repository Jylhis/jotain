# nix/options-doc.nix — Generate module option reference documentation.
#
# Evaluates each Nix module in isolation, extracts option metadata via
# nixosOptionsDoc, and assembles a self-contained HTML page.
#
# Usage:
#   nix build -f . local.checks.emacs-options-doc
#   open result/index.html
{ pkgs, src }:
let
  inherit (pkgs) lib;

  texi = import ./texi-fragment.nix;

  # Evaluate a module in isolation — no HM/NixOS/devenv stubs needed
  # because config sections are gated by mkIf cfg.enable (default false).
  evalModule =
    modulePath:
    lib.evalModules {
      modules = [
        modulePath
        { _module.check = false; }
      ];
      specialArgs = {
        inherit pkgs;
      };
    };

  # Generate CommonMark option docs for a single module.
  mkDoc =
    modulePath:
    let
      eval = evalModule modulePath;
    in
    pkgs.nixosOptionsDoc {
      options = builtins.removeAttrs eval.options [ "_module" ];
      transformOptions = opt: opt // { declarations = [ ]; };
    };

  hmDoc = mkDoc (src + "/module.nix");
  systemDoc = mkDoc (src + "/module-system.nix");

in
pkgs.runCommand "emacs-config-options-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
    # Appendix of the Info manual (nix/info-manual.nix).
    passthru.texinfoFragment = "emacs-config-options.texi";
  }
  # shellcheck disable=SC2016,SC2086
  ''
        mkdir -p $out

        cat > combined.md <<'HEADER'
    # Emacs configuration — Module Options Reference

    Auto-generated reference for all Nix module options exposed by
    the Emacs configuration.

    - [Home Manager Module](#home-manager-module) — per-user Emacs daemon, client wrapper, desktop entry
    - [System Module](#system-module-nixos) — overlay and system packages

    ---

    # Home Manager Module

    Import and enable in your Home Manager configuration:

    ```nix
    {
      imports = [ emacs-config.homeManagerModules.default ];
      services.emacsConfig = {
        enable = true;
        defaultEditor = true;
        client.enable = true;
      };
    }
    ```

    HEADER

        cat ${hmDoc.optionsCommonMark} >> combined.md

        cat >> combined.md <<'SECTION2'

    ---

    # System Module (NixOS)

    Applies the overlay and adds the package to system environment.
    For per-user daemon management, use the Home Manager module instead.

    ```nix
    {
      imports = [ emacs-config.nixosModules.default ];
      services.emacsConfig.enable = true;
    }
    ```

    SECTION2

        cat ${systemDoc.optionsCommonMark} >> combined.md

        cat > $out/style.css <<'CSS'
    :root {
      --max-width: 52rem;
      --fg: #1a1a2e;
      --bg: #fafafa;
      --accent: #7C3AED;
      --code-bg: #f0f0f5;
      --border: #e2e2e8;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --fg: #e0e0e8;
        --bg: #16161e;
        --code-bg: #1e1e28;
        --border: #2a2a3a;
      }
    }
    * { box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                   "Helvetica Neue", sans-serif;
      max-width: var(--max-width);
      margin: 2rem auto;
      padding: 0 1.5rem;
      color: var(--fg);
      background: var(--bg);
      line-height: 1.65;
    }
    h1 {
      font-size: 1.6rem;
      border-bottom: 2px solid var(--accent);
      padding-bottom: 0.4rem;
      margin-top: 2.5rem;
    }
    h1:first-of-type { margin-top: 0; }
    h2 {
      font-family: "SF Mono", "Fira Code", Menlo, Consolas, monospace;
      font-size: 1rem;
      font-weight: 600;
      margin-top: 2rem;
      padding: 0.4rem 0.6rem;
      background: var(--code-bg);
      border-left: 3px solid var(--accent);
      border-radius: 0 4px 4px 0;
      overflow-x: auto;
    }
    code {
      font-family: "SF Mono", "Fira Code", Menlo, Consolas, monospace;
      background: var(--code-bg);
      padding: 0.15rem 0.35rem;
      border-radius: 3px;
      font-size: 0.88em;
    }
    pre {
      background: var(--code-bg);
      padding: 1rem 1.2rem;
      border-radius: 6px;
      overflow-x: auto;
      border: 1px solid var(--border);
    }
    pre code { background: none; padding: 0; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    hr { border: none; border-top: 1px solid var(--border); margin: 2.5rem 0; }
    ul { padding-left: 1.5rem; }
    p { margin: 0.6rem 0; }
    CSS

        pandoc combined.md \
          -o $out/index.html \
          --standalone \
          --metadata title="Emacs configuration — Module Options" \
          --css style.css \
          --highlight-style=kate \
          --wrap=none

        # Texinfo fragment for nix/info-manual.nix. Shifting headings by one
        # turns each `# …` into an @section under docs/emacs-config.texi's
        # @appendix. See nix/texi-fragment.nix.
        pandoc combined.md \
          -f gfm \
          -t texinfo \
          --shift-heading-level-by=1 \
          --wrap=none \
        | awk '${texi.stripScaffolding}' \
        | sed -E '${texi.flattenRefs}' \
          > $out/emacs-config-options.texi

        touch $out/.nojekyll
  ''

# nix/options-doc.nix — Module option reference as an Org file.
#
# Evaluates each Nix module in isolation, extracts option metadata via
# nixosOptionsDoc and converts its CommonMark to options.org, which
# docs/jotain.org includes as the Module Options appendix.
{ pkgs, src }:
let
  inherit (pkgs) lib;

  gitHubUrl = "https://github.com/Jylhis/jotain";

  # Evaluate a module in isolation — no HM/NixOS stubs needed
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

  # Rewrite declaration paths from /nix/store/... to GitHub source links.
  transformOptions =
    opt:
    opt
    // {
      declarations = map (
        decl:
        let
          declStr = toString decl;
          srcPrefix = toString src + "/";
          rel = lib.removePrefix srcPrefix declStr;
        in
        {
          url = "${gitHubUrl}/blob/main/${rel}";
          name = rel;
        }
      ) opt.declarations;
    };

  # Generate CommonMark option docs for a single module.
  mkDoc =
    modulePath:
    let
      eval = evalModule modulePath;
    in
    pkgs.nixosOptionsDoc {
      options = builtins.removeAttrs eval.options [ "_module" ];
      inherit transformOptions;
    };

  hmDoc = mkDoc (src + "/module.nix");
  systemDoc = mkDoc (src + "/module-system.nix");

in
pkgs.runCommand "jotain-options-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
  }
  ''
    mkdir -p $out
    {
      printf '# Home Manager Module\n\n'
      cat ${hmDoc.optionsCommonMark}
      printf '\n# NixOS Module\n\n'
      cat ${systemDoc.optionsCommonMark}
    } > combined.md
    pandoc -f commonmark -t org --shift-heading-level-by=0 --wrap=none combined.md > $out/options.org
  ''

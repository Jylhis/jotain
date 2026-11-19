# Generator for ~/.config/eca/config.json. config/eca/config.json supplies
# the default OpenRouter provider; the user's `settings` deep-merge over it.
{ lib, pkgs }:
let
  jsonFormat = pkgs.formats.json { };

  base = builtins.fromJSON (builtins.readFile ../config/eca/config.json);
  openrouterProviders = { inherit (base) providers; };
in
{
  settingsType = jsonFormat.type;

  mkConfigFile =
    {
      includeOpenRouter ? true,
      settings ? { },
    }:
    jsonFormat.generate "eca-config.json" (
      lib.recursiveUpdate (lib.optionalAttrs includeOpenRouter openrouterProviders) settings
    );
}

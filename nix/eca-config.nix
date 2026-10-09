# nix/eca-config.nix — generator for the eca server config file
# (~/.config/eca/config.json, consumed by the eca-emacs client in
# lisp/init-ai.el).
#
# Home Manager only: the server reads the per-user
# $XDG_CONFIG_HOME/eca/config.json. config/eca/config.json supplies the
# default OpenRouter provider (also what `eca-models-in-sync' diffs against
# gptel); the user's freeform `settings' deep-merge over it.
{ lib, pkgs }:
let
  jsonFormat = pkgs.formats.json { };

  # Only `providers' is taken; the top-level `_comment' is dropped.
  base = builtins.fromJSON (builtins.readFile ../config/eca/config.json);
  openrouterProviders = { inherit (base) providers; };
in
{
  # `services.jotain.eca.settings': a freeform JSON tree.
  settingsType = jsonFormat.type;

  # `settings' wins over the default provider, so users can extend the
  # catalogue or override it.
  mkConfigFile =
    {
      includeOpenRouter ? true,
      settings ? { },
    }:
    jsonFormat.generate "eca-config.json" (
      lib.recursiveUpdate (lib.optionalAttrs includeOpenRouter openrouterProviders) settings
    );
}

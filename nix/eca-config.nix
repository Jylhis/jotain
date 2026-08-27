# nix/eca-config.nix — generator for the eca server config file
# (~/.config/eca/config.json, consumed by the eca-emacs client in
# lisp/init-ai.el).
#
# Only the Home Manager module (module.nix) delivers this: the eca server
# reads its global config from $XDG_CONFIG_HOME/eca/config.json, a per-user
# path a system module cannot write. This helper keeps
# config/eca/config.json the single source of truth for the default
# OpenRouter provider + model catalogue — the same file the
# `eca-models-in-sync' check (nix/checks.nix) diffs against gptel — and
# deep-merges the user's freeform `settings' over it.
{ lib, pkgs }:
let
  jsonFormat = pkgs.formats.json { };

  # The checked-in default config. Its providers.openrouter block is the
  # canonical default provider; `_comment' is documentation-only and is
  # dropped from generated output.
  base = builtins.fromJSON (builtins.readFile ../config/eca/config.json);
  openrouterProviders = { inherit (base) providers; };
in
{
  # Type for the `services.jotain.eca.settings' option: a freeform JSON
  # tree, so any eca key (providers, models, rules, mcpServers, behavior,
  # …) is expressible. Values may use eca's ${env:VAR} interpolation.
  settingsType = jsonFormat.type;

  # Render ~/.config/eca/config.json. `settings' deep-merges over the
  # default OpenRouter provider (when `includeOpenRouter'), later values
  # winning — so a user can extend the catalogue or override the provider.
  mkConfigFile =
    {
      includeOpenRouter ? true,
      settings ? { },
    }:
    jsonFormat.generate "eca-config.json" (
      lib.recursiveUpdate (lib.optionalAttrs includeOpenRouter openrouterProviders) settings
    );
}

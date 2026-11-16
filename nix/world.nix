# Where the kit helpers and the design system live, valid in two worlds:
#
#   monorepo  the in-tree platform/kit and projects/libs/design.
#   exported  the vendored jylhis/platform/kit next to the flake, and the
#             jylhis-design flake input (or the one in flake.lock).
{
  design ? null,
}:
let
  # A fetched export sits directly under the store, where `../../../..` is
  # the store dir itself and probing below it is an eval error, not `false`.
  inMonorepo =
    builtins.stringLength (toString ../../../..) > builtins.stringLength builtins.storeDir
    && builtins.pathExists ../../../../platform/kit/source.nix;
in
if inMonorepo then
  {
    kit = ../../../../platform/kit;
    design = ../../../../projects/libs/design;
  }
else
  {
    kit = ../jylhis/platform/kit;
    design = if design != null then design else (import ./locked.nix).jylhis-design;
  }

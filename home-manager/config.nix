{ inputs, config, lib, pkgs, ... }:

{
  local.fonts.monospaceFont = "Sarasa Mono J";
  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;
}

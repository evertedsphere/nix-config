{
  inputs,
  pkgs,
  ...
}: {
  local = {
    user.localUser = "s";
    fonts.monospaceFont = "Sarasa Mono J";
    programs.terminalExe = "${pkgs.alacritty}/bin/alacritty";
    gapWidth = 20;
    hasBattery = false;
    opacityHex = "80";
    opacityFloat = 0.50;
  };

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
}

{
  inputs,
  pkgs,
  ...
}: {
  local = {
    user.localUser = "s";
    programs.terminalExe = "${pkgs.alacritty}/bin/alacritty";
    gapWidth = 20;
    hasBattery = false;
    opacityHex = "C0";
    opacityFloat = 0.75;
  };

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
}

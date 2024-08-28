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
    opacityHex = "C0";
    opacityFloat = 0.75;
  };

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;
}

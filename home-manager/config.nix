{
  inputs,
  pkgs,
  ...
}: {
  local = {
    user.localUser = "s";
    fonts.monospaceFont = "Sarasa Mono J Nerd Font";
    programs.terminalExe = "${pkgs.alacritty}/bin/alacritty";
    gapWidth = 12;
  };

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;
}

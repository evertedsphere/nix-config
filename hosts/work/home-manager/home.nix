{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../../home-manager/config.nix
    ../../../home-manager/home.nix
  ];

  local = {
    user.fullName = "Soham Chowdhury";
    user.email = "soham.chowdhury@tweag.io";
    fonts.i3barFontSize = 20.0;
    fonts.alacrittyFontSize = 8.0;
    hasBattery = true;
  };

  # home.file.wallpaper.source = ../../../walls/yellow.jpg;

  home.stateVersion = "22.11";

  wayland.windowManager.sway.enable = false;

  xsession.windowManager.i3.config.fonts.size = 20.0;

  services.dunst.settings = {
    global = {
      font = "${config.stylix.fonts.monospace.name} 22";
      # TODO use scale factor?
      width = 750;
      height = 400;
    };
  };
}

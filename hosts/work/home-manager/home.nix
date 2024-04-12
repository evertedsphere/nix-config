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
  };

  home.file.wallpaper.source = ./flowers.jpg;

  home.stateVersion = "22.11";

  wayland.windowManager.sway.enable = false;

  xsession.windowManager.i3.config.fonts.size = 20.0;

  programs.swaylock = {
    enable = true;
    settings = {
      image = "~/.local/share/walls/wall.jpg";
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 60; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
      { timeout = 300; command = "${pkgs.systemd}/bin/systemctl suspend"; }
    ];
    events = [
      { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
      { event = "lock"; command = "lock"; }
    ];
  };

  services.dunst.settings = {
    global = {
      font = "${config.local.fonts.monospaceFont} 22";
      # TODO use scale factor?
      width = 750;
      height = 400;
      origin = "top-right";
      offset = "25x25";
    };
  };
}

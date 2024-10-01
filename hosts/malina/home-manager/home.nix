{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../../home-manager/config.nix
    ../../../home-manager/home.nix
    ./autorandr.nix
    ./mounts.nix
  ];

  home.stateVersion = "22.11";

  # home.file.wallpaper.source = ../../../walls/yellow.jpg;

  local = {
    user.fullName = "Soham Chowdhury";
    user.email = "evertedsphere@gmail.com";
    fonts.i3barFontSize = 13.0;
    fonts.alacrittyFontSize = 12.0;
    hasBattery = false;
  };

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
    '';
  };

  home.packages = with pkgs; [
    discord
  ];

  # xsession.windowManager.i3.config.fonts.size = 11.0;

  wayland.windowManager.sway.enable = false;

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.imagemagick}/bin/convert ${config.stylix.image} ${pkgs.i3lock}/bin/i3lock -n -t --raw 3840x2160:xbgr --image /dev/stdin";
  };

  qt.style = {
    package = pkgs.adwaita-qt;
    name = "adwaita-dark";
  };

  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Paper";
      package = pkgs.paper-icon-theme;
    };
  };

  services.dunst.settings = {
    global = {
      font = "${config.stylix.fonts.monospace.name} 14";
      # TODO use scale factor?
      width = 600;
      height = 250;
    };
  };
}

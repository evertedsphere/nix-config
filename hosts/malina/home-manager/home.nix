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
  ];

  home.stateVersion = "22.11";

  home.file.wallpaper.source = ./seto_miyako.jpg;

  local = {
    user.fullName = "Soham Chowdhury";
    user.email = "evertedsphere@gmail.com";
    fonts.i3barFontSize = 12.0;
    fonts.alacrittyFontSize = 12.0;
  };

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
    '';
  };

  home.packages = with pkgs; [
    discord
  ];

  xsession.windowManager.i3.config.fonts.size = 11.0;

  wayland.windowManager.sway.enable = false;

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
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
      font = "${config.local.fonts.monospaceFont} 14";
      # TODO use scale factor?
      width = 600;
      height = 250;
      origin = "top-right";
      offset = "25x25";
      monitor = 2;
    };
  };
}

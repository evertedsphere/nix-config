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

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
    '';
  };

  home.packages = with pkgs; [
    discord
  ];

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -i ${config.home.file.wallpaper.target}";
  };

  services.picom = {
    enable = true;
    backend = "xrender";
    fade = true;
    vSync = true;
    fadeDelta = 3;
    fadeSteps = [0.04 0.04];
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

  services.dunst = {
    enable = true;
    settings = {
      global = {
        alignment = "center";
        bounce_freq = 0;
        corner_radius = 6;
        font = config.local.fonts.monospaceFont;
        format = "<b>%s</b>\\n%b";
        geometry = "350x5-25+25";
        horizontal_padding = 8;
        idle_threshold = 120;
        ignore_newline = false;
        indicate_hidden = true;
        line_height = 0;
        markup = "full";
        max_icon_size = 60;
        padding = 8;
        separator_color = "#00000000";
        separator_height = 5;
        show_age_threshold = 30;
        sort = true;
        startup_notification = false;
        sticky_history = true;
        transparency = 0;
        word_wrap = true;
      };
    };
  };
}

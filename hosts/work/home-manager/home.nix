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
  };

  home.file.wallpaper.source = ./flowers.jpg;

  home.stateVersion = "22.11";

  wayland.windowManager.sway.enable = true;

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
}

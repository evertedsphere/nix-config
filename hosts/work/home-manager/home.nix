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
}

{ inputs, outputs, config, lib, pkgs, ... }:

{
  imports = [
    ../../../home-manager/config.nix
    ../../../home-manager/home.nix
  ];

  home.file.wallpaper.source = ./flowers.jpg;

  xsession.enable = true;

  home.stateVersion = "22.11";
}

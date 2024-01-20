{
  config,
  lib,
  pkgs,
  ...
}: {
  # allegedly fixes e.g. sort order in file pickers not being persisted
  programs.dconf.enable = true;

  # services.udev.packages = [ pkgs.libwacom ];

  environment.systemPackages = with pkgs; [
    xorg.xev
    xorg.xkbcomp
    xclip
  ];
}

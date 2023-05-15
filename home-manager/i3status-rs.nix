{ config, lib, pkgs, ... }:

{
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      # TODO nix-colors
      icons = "awesome6";
      theme = "gruvbox-dark";
      blocks = [
        {
          block = "cpu";
        }
        {
          block = "disk_space";
          path = "/";
          info_type = "available";
          interval = 20;
          warning = 20.0;
          alert = 10.0;
          format = " $icon root: $available.eng(w:2) ";
        }
        {
          block = "memory";
          format = " $icon $mem_total_used_percents.eng(w:2) ";
          format_alt = " $icon_swap $swap_used_percents.eng(w:2) ";
        }
        {
          block = "sound";
          click = [
            {
              button = "left";
              cmd = "pavucontrol";
            }
          ];
        }
        {
          block = "time";
          interval = 5;
          format = " $timestamp.datetime(f:'%a %d/%m %R') ";
        }
      ];
    };
  };
}

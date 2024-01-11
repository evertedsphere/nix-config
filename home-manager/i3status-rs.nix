{
  config,
  lib,
  pkgs,
  ...
}: let
  c = config.colorScheme.colors;
  h = x: "#${x}";
in {
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      icons = "material-nf";
      blocks = [
        {
          block = "cpu";
          format = " $icon $utilization ";
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
      settings = {
        theme = {
          overrides = {
            end_separator = "";
            separator = " / ";
            separator_bg = h c.base00;
            separator_fg = h c.base03;
            idle_bg = h c.base00;
            idle_fg = h c.base07;
            info_bg = h c.base00;
            info_fg = h c.base0D;
            good_bg = h c.base00;
            good_fg = h c.base0B;
            warning_bg = h c.base00;
            warning_fg = h c.base0A;
            critical_bg = h c.base00;
            critical_fg = h c.base08;
          };
        };
      };
    };
  };
}

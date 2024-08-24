{
  config,
  lib,
  pkgs,
  ...
}: let
  c = config.colorScheme.palette;
  h = x: "#${x}";
in {
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      icons = "material-nf";
      blocks = [
        {
          block = "music";
          format = "{$combo.str(max_w:70)|}";
        }
        {
          block = "sound";
          device_kind = "source";
          format = "mic {$volume.eng(w:2)|mute}";
          click = [
            {
              button = "left";
              cmd = "pavucontrol";
            }
          ];
        }
        {
          block = "sound";
          device_kind = "sink";
          format = "snd {$volume.eng(w:2)|mute}";
          click = [
            {
              button = "left";
              cmd = "pavucontrol";
            }
          ];
        }
        {
          block = "cpu";
          format = "cpu $utilization";
        }
        {
          block = "memory";
          format = "mem $mem_total_used_percents.eng(w:2)";
          format_alt = "swp $swap_used_percents.eng(w:2)";
        }
        {
          block = "disk_space";
          path = "/";
          info_type = "available";
          interval = 20;
          warning = 20.0;
          alert = 10.0;
          format = "root $percentage.eng(w:2)";
        }
        {
          block = "time";
          interval = 5;
          format = "$timestamp.datetime(f:'%a %d/%m %R')";
        }
        (lib.mkIf config.local.hasBattery
          {
            block = "battery";
            interval = 60;
          })
      ];
      settings = {
        theme = {
          overrides = let
            ht = colour_with_transparency config.local.opacityHex;
            # zero
            hz = colour_with_transparency "00";
            colour_with_transparency = tr: x: "#${x}${tr}";
          in {
            separator = " / ";
            end_separator = " ";
            separator_bg = ht c.base00;
            separator_fg = h c.base02;
            idle_bg = ht c.base00;
            idle_fg = h c.base05;
            info_bg = ht c.base00;
            info_fg = h c.base0D;
            good_bg = ht c.base00;
            good_fg = h c.base0B;
            warning_bg = ht c.base00;
            warning_fg = h c.base0A;
            critical_bg = ht c.base00;
            critical_fg = h c.base08;
          };
        };
      };
    };
  };
}

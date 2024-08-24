{
  config,
  lib,
  pkgs,
  ...
}: let
  c = config.colorScheme.palette;
  h = x: "#${x}";
in {
  programs.alacritty = {
    enable = true;
    settings = {
      # shell.program = "${pkgs.nushell}/bin/nu";
      colors = {
        primary = {
          background = h c.base00;
          foreground = h c.base05;
        };
        cursor = {
          text = h c.base00;
          cursor = h c.base05;
        };
        normal = {
          black = h c.base00;
          red = h c.base08;
          green = h c.base0B;
          yellow = h c.base0A;
          blue = h c.base0D;
          magenta = h c.base0E;
          cyan = h c.base0C;
          white = h c.base05;
        };
        bright = {
          black = h c.base03;
          red = h c.base08;
          green = h c.base0B;
          yellow = h c.base0A;
          blue = h c.base0D;
          magenta = h c.base0E;
          cyan = h c.base0C;
          white = h c.base07;
        };
        indexed_colors = [
          {
            index = 16;
            color = h c.base09;
          }
          {
            index = 17;
            color = h c.base0F;
          }
          {
            index = 18;
            color = h c.base01;
          }
          {
            index = 19;
            color = h c.base02;
          }
          {
            index = 20;
            color = h c.base04;
          }
          {
            index = 21;
            color = h c.base06;
          }
        ];
      };
      window = {
        opacity = config.local.opacityFloat;
        dimensions = {
          columns = 100;
          lines = 85;
        };
        padding = {
          # i3 gap width
          x = config.local.gapWidth;
          y = config.local.gapWidth;
        };
        dynamic_padding = false;
        decorations = "None";
        startup_mode = "Windowed";
      };

      scrolling = {
        history = 10000;
        multiplier = 3;
      };

      font = {
        normal = {
          family = config.local.fonts.monospaceFont;
        };
        size = config.local.fonts.alacrittyFontSize;
      };
    };
  };
}

{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: let colors = config.colorScheme.colors; in {
  imports = [
    # outputs.homeManagerModules.example
    ./hyprland.nix
    inputs.nix-colors.homeManagerModules.default
  ];

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      # neovim-nightly-overlay.overlays.default
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "s";
    homeDirectory = "/home/s";
  };

programs.wofi.enable = true;
  programs.qutebrowser = {
    enable = true;
  };

  services.gammastep = {
    enable = true;
    provider = "geoclue2";
    temperature = {
      day = 6000;
      night = 4600;
    };
    settings = {
      general.adjustment-method = "wayland";
    };
  };



  programs.neovim.enable = true;
  home.packages = with pkgs; [swaybg];
  programs.alacritty = {
    enable = true;
    settings = {
      colors = {
	primary = {
          background = "#${colors.base00}";
          foreground = "#${colors.base05}";
        };
        cursor = {
          text = "#${colors.base00}";
          cursor = "#${colors.base05}";
        };
        normal = {
          black = "#${colors.base00}";
          red = "#${colors.base08}";
          green = "#${colors.base0B}";
          yellow = "#${colors.base0A}";
          blue = "#${colors.base0D}";
          magenta = "#${colors.base0E}";
          cyan = "#${colors.base0C}";
          white = "#${colors.base05}";
        };
        bright = {
          black = "#${colors.base03}";
          red = "#${colors.base08}";
          green = "#${colors.base0B}";
          yellow = "#${colors.base0A}";
          blue = "#${colors.base0D}";
          magenta = "#${colors.base0E}";
          cyan = "#${colors.base0C}";
          white = "#${colors.base07}";
        };
        indexed_colors = [
          {
            index = 16;
            color = "#${colors.base09}";
          }
          {
            index = 17;
            color = "#${colors.base0F}";
          }
          {
            index = 18;
            color = "#${colors.base01}";
          }
          {
            index = 19;
            color = "#${colors.base02}";
          }
          {
            index = 20;
            color = "#${colors.base04}";
          }
          {
            index = 21;
            color = "#${colors.base06}";
          }
        ];
      };
      window = {
        opacity = 0.93;
        dimensions = {
          columns = 100;
          lines = 85;
        };
        padding = {
          # i3 gap width
          x = 12;
          y = 12;
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
        normal = {family = "Iosevka";};
        size = 12.0;
      };
    };
  };

  programs.home-manager.enable = true;
  programs.git.enable = true;
  programs.atuin.enable = true;

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  programs.zsh = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
  };

  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 32;
        output = [
          "DP-2"
          "HDMI-A-2"
        ];
        modules-left = ["wlr/workspaces" "wlr/taskbar"];
        modules-center = [];
        modules-right = ["temperature"];

        "wlr/workspaces" = {
          disable-scroll = true;
          all-outputs = false;
        };
      };
    };
  };


  systemd.user.startServices = "sd-switch";

  home.stateVersion = "22.11";
}

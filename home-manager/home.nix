# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "s";
    homeDirectory = "/home/s";
  };

  programs.neovim.enable = true;
  # home.packages = with pkgs; [ steam ];
  programs.alacritty = {
    enable = true;
    settings = {
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
          normal = { family = "Iosevka"; };
          size = 12.0;
        };
      };
    };

  programs.home-manager.enable = true;
  programs.git.enable = true;

  wayland.windowManager.hyprland = {
    enable = true;
    nvidiaPatches = true;
    xwayland.enable = true;
    extraConfig = ''
      $mod = SUPER 
      general {
	      gaps_in = 14
	      gaps_out = 28
	      border_size = 2
      }

monitor=DP-2,preferred,auto,1.6
monitor=HDMI-A-2,preferred,auto,1.3

    # terminal
    bind = $mod, Return, exec, ${pkgs.alacritty}/bin/alacritty
    '';
  };

  systemd.user.startServices = "sd-switch";

  home.stateVersion = "22.11";
}

{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../../home-manager/config.nix
    ../../../home-manager/home.nix
  ];

  home.stateVersion = "22.11";

  home.file.wallpaper.source = ./seto_miyako.jpg;

  local = {
    user.fullName = "Soham Chowdhury";
    user.email = "evertedsphere@gmail.com";
    fonts.i3barFontSize = 12.0;
    fonts.alacrittyFontSize = 12.0;
  };

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
    '';
  };

  home.packages = with pkgs; [
    discord
  ];

  xsession.windowManager.i3.config.fonts.size = 11.0;

  wayland.windowManager.sway.enable = false;

  services.autorandr.enable = true;
  programs.autorandr = {
    enable = true;
    profiles = {
      "base" = {
        fingerprint = {
          DP-2 = "00ffffffffffff004c2d960f503657431a1e0104b54627783ace51a6574c9f26125054bfef80714f810081c08180a9c0b300950001014dd000a0f0703e8030203500b9882100001a000000fd001e4b1e873c000a202020202020000000fc00553332523539780a2020202020000000ff0048345a4e3630303234330a2020014e02030ff042105f2309070783010000023a801871382d40582c4500b9882100001e565e00a0a0a0295030203500b9882100001a04740030f2705a80b0588a00b9882100001e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000056";
          HDMI-0 = "00ffffffffffff0054e40224030920200d1d010380351e78eeee91a3544c99260f5054a54b00b300d100714fa9408180778001010101565e00a0a0a02950302035000f282100001a000000fc004172746973742032342050726f00000010004172746973742032345250726f000000fd0038561e711e000a20202020202001c7020328f14f1005040302071601141f1213202122230907078301000067030c001000383de3050301023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e96005550210000180000000000000000000000000000004d";
          HDMI-1 = "00ffffffffffff004c2d940f355538300e210103804627782ace51a6574c9f26125054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a00b9882100001e000000fd00184b1e873c000a202020202020000000fc00553332523539780a2020202020000000ff00484b32573330313031320a20200151020334f04d611203130420221f105f605d5e23090707830100006d030c001000b84420006001020367d85dc40178800be30f0104023a801871382d40582c4500b9882100001e023a80d072382d40102c4580b9882100001e04740030f2705a80b0588a00b9882100001e565e00a0a0a0295030203500b9882100001a00000090";
        };
        config = {
          DP-0.enable = false;
          DP-1.enable = false;
          DP-3.enable = false;
          DP-4.enable = false;
          DP-5.enable = false;

          HDMI-1 = {
            enable = true;
            crtc = 2;
            mode = "3840x2160";
            position = "0x0";
            rate = "60.00";
            # x-prop-non_desktop 0
          };

          DP-2 = {
            enable = true;
            primary = true;
            crtc = 0;
            mode = "3840x2160";
            position = "3840x0";
            rate = "60.00";
          };

          HDMI-0 = {
            enable = true;
            crtc = 1;
            mode = "2560x1440";
            position = "3840x2160";
            rate = "59.95";
          };
        };
        hooks = {
          postswitch = "systemctl --user restart random-background.service";
        };
      };
    };
  };

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
  };

  qt.style = {
    package = pkgs.adwaita-qt;
    name = "adwaita-dark";
  };

  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Paper";
      package = pkgs.paper-icon-theme;
    };
  };

  services.dunst.settings = {
    global = {
      font = "${config.local.fonts.monospaceFont} 14";
      # TODO use scale factor?
      width = 600;
      height = 250;
      origin = "top-right";
      offset = "25x25";
      monitor = 2;
    };
  };
}

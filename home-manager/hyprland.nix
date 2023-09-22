{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    xorg.xprop
    (inputs.hyprland-contrib.packages.${pkgs.hostPlatform.system}.grimblast)
  ];

  # start swayidle as part of hyprland, not sway
  #systemd.user.services.swayidle.Install.WantedBy = lib.mkForce ["hyprland-session.target"];

  wayland.windowManager.hyprland = {
    enable = true;
    enableNvidiaPatches = true;
    xwayland.enable = true;
    extraConfig = ''
      $mod = SUPER

      # nvidia tweaks
      env = LIBVA_DRIVER_NAME,nvidia
      env = XDG_SESSION_TYPE,wayland
      # prevent cursor disappearing
      env = WLR_NO_HARDWARE_CURSORS,1
      # these two may have to be disabled later
      env = GBM_BACKEND,nvidia-drm
      env = __GLX_VENDOR_LIBRARY_NAME,nvidia

      # ~133ppi
      monitor=HDMI-A-1,3840x2160,0x0,1.5
      monitor=DP-2,3840x2160,2560x0,1.5
      # ~127ppi
      monitor=HDMI-A-2,2560x1440,2560x1440,1.4
      monitor=,preferred,auto,1

      #exec-once = xprop -root -f _XWAYLAND_GLOBAL_OUTPUT_SCALE 32c -set _XWAYLAND_GLOBAL_OUTPUT_SCALE 1.4
      exec-once = ${pkgs.swaybg}/bin/swaybg -i ~/.local/share/walls/wall.jpg

      # set cursor for HL itself
      exec-once = hyprctl setcursor ${config.home.pointerCursor.name} ${toString config.home.pointerCursor.size}

      # move focus
      bind = $mod, h, movefocus, l
      bind = $mod, l, movefocus, r
      bind = $mod, k, movefocus, u
      bind = $mod, j, movefocus, d

      # window resize
      bind = $mod, o, submap, resize
      submap = resize
      binde = , h, resizeactive, 40 0
      binde = , l, resizeactive, -40 0
      binde = , k, resizeactive, 0 -40
      binde = , j, resizeactive, 0 40
      bind = , escape, submap, reset
      submap = reset

      general {
      gaps_in = 8
      gaps_out = 16
      border_size = 2
      }

      # terminal
      bind = $mod, Return, exec, ${config.local.programs.terminalExe}
      bind = $mod, p, exec, wofi --show drun


      animations {
      enabled = false
      animation = border, 1, 1, default
      animation = fade, 1, 1, default
      animation = windows, 1, 1, default
      animation = workspaces, 1, 1, default
      }

      decoration {
        rounding = 2
        # blur = true
        # blur_size = 3
        # blur_passes = 3
        # drop_shadow = true
        # shadow_ignore_window = true
        # shadow_offset = 0 5
        # shadow_range = 50
        # shadow_render_power = 3
        # col.shadow = rgba(00000099)
      }


      # only allow shadows for floating windows
      windowrulev2 = noshadow, floating:0
      # hide sharing indicators
      windowrulev2 = workspace special silent, title:^(Firefox — Sharing Indicator)$
      windowrulev2 = workspace special silent, title:^(.*is sharing (your screen|a window)\.)$


      # workspaces
      # binds mod + [shift +] {1..10} to [move to] ws {1..10}
      ${builtins.concatStringsSep "\n" (builtins.genList (
          x: let
            ws = let
              c = (x + 1) / 10;
            in
              builtins.toString (x + 1 - (c * 10));
          in ''
            bind = $mod, ${ws}, workspace, ${toString (x + 1)}
            bind = $mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
          ''
        )
        10)}
    '';
  };
}

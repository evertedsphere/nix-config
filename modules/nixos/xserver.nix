{
  config,
  lib,
  pkgs,
  ...
}: {
  # allegedly fixes e.g. sort order in file pickers not being persisted
  programs.dconf.enable = true;

  services.picom = let
    special = [
      "window_type *= 'menu'"
      "window_type *= 'notification'"
      "window_type *= 'utility'"
      "bounding_shaped && !rounded_corners"];
  in {
    backend = "glx";
    fade = true;
    vSync = true;
    fadeDelta = 6;
    fadeSteps = [0.028 0.032];
    fadeExclude = special;
    shadow = true;
    # shadowExclude = special ++ ["class_i = 'Dunst'"];
    shadowExclude = special;
    settings = {
      # https://github.com/dunst-project/dunst/issues/697#issuecomment-1188107553
      # prevents notifs from showing on lockscreen
      # unredir-if-possible = true;
      corner-radius = 8;
      shadow-offset-x = -20;
      shadow-offset-y = -20;
      shadow-radius = 20;
      shadow-opacity = 0.8;
      rounded-corners-exclude = special;
      blur = {
        method = "dual_kawase";
        strength = 8;
      };
      blur-background-exclude = special;
    };
  };

  services.libinput.enable = true;

  services.xserver = {
    # wacom.enable = true;
    digimend.enable = true;
    # "This option enable propagating /etc/X11/xkb symlink, which is standard include path for xkbcomp."
    # fsr extralayouts disables this unless you're using startx
    # exportConfiguration = lib.mkForce true;
    # ideally i would be able to set these in the bqn module... but maybe not
    xkb.layout = "us,apl";
    xkb.variant = ",dyalog";
    xkb.options = "grp:switch,ctrl:nocaps,compose:menu";
    desktopManager.session = [
      {
        name = "home-manager";
        bgSupport = true;
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession
          waitPID=$!
        '';
      }
    ];
    displayManager = {
      lightdm.enable = true;
    };
  };

  # TODO if xserver
  environment.systemPackages = with pkgs; [
    xorg.xev
    xorg.xkbcomp
    xclip
  ];
}

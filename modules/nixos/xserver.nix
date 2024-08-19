{
  config,
  lib,
  pkgs,
  ...
}: {
  # allegedly fixes e.g. sort order in file pickers not being persisted
  programs.dconf.enable = true;

  services.picom = {
    backend = "xrender";
    fade = true;
    vSync = true;
    fadeDelta = 3;
    fadeSteps = [0.04 0.04];
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
      setupCommands = ''
        # xrandr --output HDMI-0 --mode 2560x1440 --pos 3840x2160 --rotate normal --output DP-2 --mode 3840x2160 --pos 3840x0 --rotate normal --output HDMI-1 --mode 3840x2160 --pos 0x0 --rotate normal
      '';
    };
  };

  # TODO if xserver
  environment.systemPackages = with pkgs; [
    xorg.xev
    xorg.xkbcomp
    xclip
  ];
}

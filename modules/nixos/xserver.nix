{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./xserver/keyd.nix
  ];

  # allegedly fixes e.g. sort order in file pickers not being persisted
  programs.dconf.enable = true;

  services.udev.packages = [ pkgs.libwacom ];

  services.xserver = {
    wacom.enable = true;
    enable = true;
    # # "This option enable propagating /etc/X11/xkb symlink, which is standard include path for xkbcomp."
    # # fsr extralayouts disables this unless you're using startx
    # exportConfiguration = lib.mkForce true;
    libinput.enable = true;
    # ideally i would be able to set these in the bqn module... but maybe not
    layout = "us,apl";
    xkbVariant = ",dyalog";
    xkbOptions = "grp:switch,ctrl:nocaps,compose:menu";
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

  environment.systemPackages = with pkgs; [
    xorg.xev
    xorg.xkbcomp
    xclip
  ];
}

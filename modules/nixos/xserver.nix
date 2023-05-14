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

  services.xserver = {
    enable = true;
    # "This option enable propagating /etc/X11/xkb symlink, which is standard include path for xkbcomp."
    # exportConfiguration = true;
    libinput.enable = true;
    layout = "us";
    extraLayouts.bqn = {
      description = "bqn symbols, available in xkeyboard-config >= 2.36";
      # languages = [ "bqn" ];
      symbolsFile = ./xserver/bqn;
    };
    xkbOptions = "ctrl:nocaps,compose:menu";
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

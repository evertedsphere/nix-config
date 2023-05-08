{ config, lib, pkgs, ... }:

{
  # allegedly fixes e.g. sort order in file pickers not being persisted
  programs.dconf.enable = true;
  services.xserver = {
    enable = true;
    libinput.enable = true;
    layout = "us";
    xkbOptions = "eurosign:e,ctrl:nocaps,compose:ralt";
    desktopManager.session = [{
      name = "home-manager";
      bgSupport = true;
      start = ''
        ${pkgs.runtimeShell} $HOME/.hm-xsession
        waitPID=$!
      '';
    }];
    displayManager = { lightdm.enable = true; };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}: {
  sound.enable = false;
  hardware.bluetooth.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.support32Bit = true;
    alsa.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };
}

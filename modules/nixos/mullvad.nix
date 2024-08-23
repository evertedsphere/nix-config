{
  pkgs,
  config,
  ...
}: {
  # Done for us by the service
  # FIXME https://github.com/NixOS/nixpkgs/issues/113589
  networking.firewall.checkReversePath = "loose";

  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
  };
  # When a "strings concatenated with \n" input is required, you can do this:
  systemd.services."mullvad-daemon".postStart = let
    script = pkgs.resholve.writeScriptBin "set-up-mullvad" {
      inputs = with pkgs; [config.services.mullvad-vpn.package coreutils];
      interpreter = "${pkgs.bash}/bin/bash";
    } (builtins.readFile ./set-up-mullvad.sh);
  in "${script}/bin/set-up-mullvad";
  networking.wireguard.enable = true;
}

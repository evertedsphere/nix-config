{pkgs, ...}: {
  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
  };
  # FIXME https://github.com/NixOS/nixpkgs/issues/113589
  networking.firewall.checkReversePath = "loose";
  networking.wireguard.enable = true;
}

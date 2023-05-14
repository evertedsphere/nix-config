{pkgs, ...}: {
  services.mullvad-vpn.enable = true;
  # FIXME https://github.com/NixOS/nixpkgs/issues/113589
  networking.firewall.checkReversePath = "loose";
  networking.wireguard.enable = true;
  environment.systemPackages = [pkgs.mullvad-vpn];
}

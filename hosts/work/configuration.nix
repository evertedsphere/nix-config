{
  config,
  lib,
  pkgs,
  ...
}: let
  malinaKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCe63AsifSSBDJ5mk9IrTKkAUonBaoK8klnX222tpgg1xOGNzQrttxQPzCl10hT9eqbHCk6KIdV2iWF4xe09/4E++roXcq3SX0IBedQWPyhuPQYsfA0Titvkv6WKZ45j6dubAL6DfqJaIL4eWPobGlA27uLdh70r/cOLbpeS7uIHVvc0pGp/3OQrtZEYY9sKjeh5xl7MKrHH4vUhEaeQKRofkEr8+mr7zdjO4LoEwIwUNFdYAhTljgbpGl+UU2g1ZJ1+dhdMzgDLZdtyA59rkKRE8A1/jPoBN2pSvbgZM/sgCsaKJfowifM330g1Nm5HkX/nl/v2BTdoUMz0FHMmG0TekqgrZlbYn+D3jaZY7maf9C2l+KVPoZE6knVw0UmcfZOM/u+IxfPLmrzW7/fcWlLVbEJ/TaPNSLOBsT/2HFYMT0Tx9DJWJFYSC4PhYpZNXFZaxSSGi/B2ruTNSteY017VeSFHz9orbfALnStYpnTwOnbR3s8lqmvIFxU3AtKDbk= s@malina";
in {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.initrd.luks.devices."luks-49f9bfd8-a480-4bb5-9ec6-26483d5bae0f".device = "/dev/disk/by-uuid/49f9bfd8-a480-4bb5-9ec6-26483d5bae0f";
  networking.hostName = "work";

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
  };

  nix.settings = {
    substituters = [
      "https://storage.googleapis.com/zeuslogics-nix-cache-github"
    ];
    trusted-public-keys = [
      "zeuslogics-nix-cache-github:RpfcOgIp6w2cvPyhTfErGcWkR9QSHc1gpp4UwyH3ovU="
    ];
  };

  users.users.root = {
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [malinaKey];
  };

  users.users.s = {
    isNormalUser = true;
    description = "Soham Chowdhury";
    extraGroups = ["networkmanager" "wheel" "docker"];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [malinaKey];
  };

  system.stateVersion = "23.11";
}

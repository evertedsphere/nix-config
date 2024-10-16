{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  ...
}: let
  malinaKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCe63AsifSSBDJ5mk9IrTKkAUonBaoK8klnX222tpgg1xOGNzQrttxQPzCl10hT9eqbHCk6KIdV2iWF4xe09/4E++roXcq3SX0IBedQWPyhuPQYsfA0Titvkv6WKZ45j6dubAL6DfqJaIL4eWPobGlA27uLdh70r/cOLbpeS7uIHVvc0pGp/3OQrtZEYY9sKjeh5xl7MKrHH4vUhEaeQKRofkEr8+mr7zdjO4LoEwIwUNFdYAhTljgbpGl+UU2g1ZJ1+dhdMzgDLZdtyA59rkKRE8A1/jPoBN2pSvbgZM/sgCsaKJfowifM330g1Nm5HkX/nl/v2BTdoUMz0FHMmG0TekqgrZlbYn+D3jaZY7maf9C2l+KVPoZE6knVw0UmcfZOM/u+IxfPLmrzW7/fcWlLVbEJ/TaPNSLOBsT/2HFYMT0Tx9DJWJFYSC4PhYpZNXFZaxSSGi/B2ruTNSteY017VeSFHz9orbfALnStYpnTwOnbR3s8lqmvIFxU3AtKDbk= s@malina";
in {
  imports = [
    ./hardware-configuration.nix
    outputs.nixosModules.xserver
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
  ];

  boot.initrd.luks.devices."luks-49f9bfd8-a480-4bb5-9ec6-26483d5bae0f".device = "/dev/disk/by-uuid/49f9bfd8-a480-4bb5-9ec6-26483d5bae0f";
  networking.hostName = "work";

  nix.settings = {
    substituters = [
      "https://storage.googleapis.com/zeuslogics-nix-cache-github"
    ];
    trusted-public-keys = [
      "zeuslogics-nix-cache-github:RpfcOgIp6w2cvPyhTfErGcWkR9QSHc1gpp4UwyH3ovU="
    ];
  };

  # security.pam.services.swaylock = {};

  services.xserver.enable = true;
  services.picom.enable = true;

  users.users.root = {
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [malinaKey];
  };

  users.users.s = {
    isNormalUser = true;
    description = "Soham Chowdhury";
    extraGroups = ["networkmanager" "wheel" "docker" "keyd"];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [malinaKey];
  };

  environment.sessionVariables.FLAKE = "/home/s/nix-config";

  system.stateVersion = "23.11";
}

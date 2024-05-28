{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: let
  zdradaSshKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCwG0xwG+Q73dHW5M7Yyos1ns7DdNMZ4Vho8AKueTG116wLe92JChjxg7+cOzit026zz1Ni+B2/jS9/RF3WZAVWpTjFv2c3DaCy1TR/LlOqWp4qZJMmJBtymQ83wm0p49ELIkY5XOw3xtZKi3PurKa1yo2gbGnu7u91Tm4LP/rOi52F6vJFR28OR2O5HuQeu48zEQE2BHXfd0tBJt2bMS+2wRYwKdz02XUS7bpSK/8EC7Dou/El7Vm3faqIuQk5/63kxc4LZVHq7IAhcRYYZWOdEeBWat7AFDA3w/8upAdWQrZBh6X+XnGclRgNJAzU4QJ+Vkp8UqHFbrMy82b4QyHAo1pS1/VWU6lN5A8ccVbJYzZWRpT+Nijj1nJepeRsqE7xKDjMyfAEFiUCApoalCB/Qcout6fQFOn/bOa/1EYlHZh6jppu5Fpl4ZxTshpZgAwC7cNp2O4r9K6l0Cslt5fz0Hfq3Y/+1Y/soPg0BH9YwluXKvhJJAbHME2WNGlmkVE= k@zdrada";
in {
  imports = [
    outputs.nixosModules.xserver
    outputs.nixosModules.mullvad
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.impermanence.nixosModule
    ./hardware-configuration.nix
  ];

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    config.common.default = "*";
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };
  services.xserver.enable = true;
  services.picom.enable = true;

  boot.supportedFilesystems = ["zfs" "ntfs"];
  networking.hostId = "44a15ee1";
  boot.zfs.package = pkgs.zfs_unstable;
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/742e9b2a-dd82-4e82-b558-9508ccb6c9da";
  # boot.initrd.luks.devices."cryptdata".device = "/dev/disk/by-uuid/9ffbf99f-97b3-4931-9fbe-259a2b6498f3";
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';
  services.sanoid = {
    enable = true;
    datasets = {
      "rpool/safe" = {
        autoprune = true;
        autosnap = true;
        hourly = 10;
        daily = 7;
        monthly = 2;
        yearly = 0;
        recursive = "zfs";
      };
    };
  };

  networking.hostName = "malina";
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 34340;
      to = 34350;
    }
  ];
  networking.firewall.allowedUDPPortRanges = [
    {
      from = 34340;
      to = 34350;
    }
  ];

  environment.systemPackages = with pkgs; [
    goldendict-ng
    anki
    (gallery-dl.overrideAttrs {
      # for this patch
      # https://github.com/mikf/gallery-dl/commit/347af7f5c8423ce0f2ee9119d37143c0e319b590
      src = pkgs.fetchFromGitHub {
        owner = "mikf";
        repo = "gallery-dl";
        rev = "ea434963aed0cfb9c9a6fd412c42631cf5091e70";
        sha256 = "sha256-66L5FH5zsNzUvALLNiJv21DlhOOl+FSNiWNun4zoH3w=";
      };
      patches = [];
    })
    (pkgs.makeDesktopItem {
      name = "pureref";
      exec = "${pkgs.pureref}/bin/pureref";
      comment = "Reference manager";
      desktopName = "Pureref";
      type = "Application";
      mimeTypes = [];
    })
  ];

  # ---------------------------------------------------------
  # impermanence

  fileSystems."/persist".neededForBoot = true;
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/root"
      "/etc/nixos"
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = with pkgs; [
    fcitx5-mozc
    fcitx5-gtk
    fcitx5-rime
  ];

  users.users.root = {
    initialPassword = "hunter2";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [zdradaSshKey];
  };

  users.users.s = {
    isNormalUser = true;
    extraGroups = ["networkmanager" "wheel" "docker" "keyd"];
    initialPassword = "hunter2";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [zdradaSshKey];
  };

  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.powerManagement.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  virtualisation.docker.enableNvidia = true;

  services.openssh = {
    settings = {
      PermitRootLogin = "without-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  environment.sessionVariables.FLAKE = "/persist/nix-config";

  system.stateVersion = "22.11";
}

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
    outputs.nixosModules.fonts
    outputs.nixosModules.xserver
    outputs.nixosModules.audio
    outputs.nixosModules.mullvad
    outputs.nixosModules.bqn
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    ./hardware-configuration.nix
    inputs.impermanence.nixosModule
  ];

  nixpkgs = {
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # neovim-nightly-overlay.overlays.default
    ];

    config = {
      allowUnfree = true;
    };
  };



  # boot.kernelPatches = [
  #   {
  #     name = "enable RT_FULL";
  #     patch = null;
  #     extraConfig = ''
  #       PREEMPT y
  #       PREEMPT_BUILD y
  #       PREEMPT_VOLUNTARY n
  #       PREEMPT_COUNT y
  #       PREEMPTION y
  #     '';
  #   }
  # ];

  services.flatpak.enable = true;

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      substituters = [ "https://ghc-nix.cachix.org" ];
      trusted-public-keys = [ "ghc-nix.cachix.org-1:wI8l3tirheIpjRnr2OZh6YXXNdK2fVQeOI4SVz/X8nA=" ];
      trusted-users = [ "s" "root" ];
    };
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["zfs" "ntfs"];
  networking.hostId = "44a15ee1";
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest; # config.boot.zfs.package.latestCompatibleLinuxPackages;
  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/742e9b2a-dd82-4e82-b558-9508ccb6c9da";
  # boot.initrd.luks.devices."cryptdata".device = "/dev/disk/by-uuid/9ffbf99f-97b3-4931-9fbe-259a2b6498f3";
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';

  networking.hostName = "malina";
  networking.networkmanager.enable = true;

  # this also governs wayland
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.powerManagement.enable = true;
  # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
  };

  security = {
    # allow wayland lockers to unlock the screen
    #pam.services.swaylock.text = "auth include login";

    # userland niceness
    rtkit.enable = true;
    sudo.wheelNeedsPassword = false;
  };

  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  time.timeZone = "Europe/Paris";

  # ---------------------------------------------------------
  # impermanence

  fileSystems."/persist".neededForBoot = true;
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/root"
      "/etc/nixos"
      "/etc/NetworkManager/system-connections"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

  # services.xserver.enable = true;
  services.arbtt = {
    enable = true;
    sampleRate = 10;
  };

  zramSwap.enable = true;
  services.earlyoom = {
    freeMemThreshold = 5;
    freeSwapThreshold = 10;
    enableNotifications = true;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';

  console = {
    earlySetup = true;
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };

  programs.zsh.enable = true;
  environment.pathsToLink = ["/share/zsh"];
  # this is needed or else lightdm doesn't show the user bc it thinks any user
  # with a shell not in /etc/shells is a system user
  environment.shells = with pkgs; [bashInteractive zsh];

  boot.kernelModules = ["uinput"];
  # for kmonad, but i'll leave it in
  services.udev.extraRules = ''
    KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput", GROUP="input", MODE="0660"
  '';
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = with pkgs; [fcitx5-mozc fcitx5-gtk];

  users.users.root = {
    initialPassword = "hunter2";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      zdradaSshKey
    ];
  };

  users.users.s = {
    isNormalUser = true;
    extraGroups = ["wheel" "docker"];
    initialPassword = "hunter2";
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      zdradaSshKey
    ];
  };

  virtualisation.docker = {
    enable = true;
    enableNvidia = true;
  };

  environment.systemPackages = with pkgs; [
    firefox
    godot_4
    krita
    docker-compose

    git
    # compilers and interpreters
    j
    gnuplot
    rustup
    nodejs
    # terminal utilities
    parallel
    rlwrap
    fd
    exa
    bat
    delta
    # data wrangling
    ripgrep
    jq
    miller
    # net
    wget
    htop
    lsof
    # formatters
    alejandra

    neovim
    (pkgs.emacsWithPackages (epkgs: [
      # for org-roam, which fails to function otherwise
      # also see note in packages.el
      epkgs.emacsql-sqlite
      epkgs.org-roam
    ]))
  ];

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "without-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  system.stateVersion = "22.11";
}

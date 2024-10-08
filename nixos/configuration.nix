{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = with outputs.nixosModules; [
    keyd
    fonts
    audio
    bqn
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      # outputs.overlays.unstable-packages
      inputs.emacs-overlay.overlays.default
      inputs.wired-notify.overlays.default
    ];

    config = {allowUnfree = true;};
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Paris";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_GB.UTF-8";
      LC_IDENTIFICATION = "en_GB.UTF-8";
      LC_MEASUREMENT = "en_GB.UTF-8";
      LC_MONETARY = "fr_FR.UTF-8";
      LC_NAME = "en_GB.UTF-8";
      LC_NUMERIC = "en_GB.UTF-8";
      LC_PAPER = "en_GB.UTF-8";
      LC_TELEPHONE = "en_GB.UTF-8";
      LC_TIME = "en_GB.UTF-8";
    };
  };

  nix = {
    optimise.automatic = true;
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = rec {
      experimental-features = "nix-command flakes";
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      # These are trusted, but won't be hit by default.
      trusted-substituters =
        substituters
        ++ [
          "https://storage.googleapis.com/zeuslogics-nix-cache-github"
          "https://ghc-nix.cachix.org"
          "https://cuda-maintainers.cachix.org"
          "https://ploop.cachix.org"
        ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "ghc-nix.cachix.org-1:wI8l3tirheIpjRnr2OZh6YXXNdK2fVQeOI4SVz/X8nA="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "zeuslogics-nix-cache-github:RpfcOgIp6w2cvPyhTfErGcWkR9QSHc1gpp4UwyH3ovU="
        "ploop.cachix.org-1:i6+Fqarsbf5swqH09RXOEDvxy7Wm7vbiIXu4A9HCg1g="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
      trusted-users = ["s" "root"];
    };
  };

  security = {
    # for sway
    polkit.enable = true;
    sudo.wheelNeedsPassword = false;
  };

  services.arbtt = {
    enable = true;
    sampleRate = 10;
  };

  stylix = {
    enable = true;
    # not sure why we need a default
    image = ../walls/yellow.jpg;

    fonts = {
      serif = {
        package = pkgs.dejavu_fonts;
        name = "IBM Plex Serif";
      };

      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "IBM Plex Sans";
      };

      monospace = {
        package = pkgs.iosevka-comfy.comfy;
        name = "Iosevka Comfy";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Emoji";
      };
    };
  };

  zramSwap.enable = true;

  services.earlyoom = {
    freeMemThreshold = 5;
    freeSwapThreshold = 10;
    enableNotifications = true;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=30s
    DefaultTimeoutStopSec=30s
  '';

  console = {
    earlySetup = true;
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = false;
  };
  environment.pathsToLink = ["/share/zsh"];
  environment.shells = with pkgs; [bashInteractive zsh];
  users.defaultUserShell = pkgs.zsh;

  boot.kernelModules = ["uinput"];

  boot.kernel.sysctl."kernel.sysrq" = 1;

  services.dbus.packages = [pkgs.blueman];
  services.blueman.enable = true;
  programs.kdeconnect.enable = true;

  virtualisation.docker = {
    enable = true;
  };

  # Thumbnails
  services.tumbler.enable = true;

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 10d --keep 10";
  };

  environment.systemPackages = with pkgs; [
    nix-output-monitor
    nh

    keyd
    # vulkan-validation-layers

    zotero_7
    pandoc
    gifsicle
    scrot
    imagemagick
    tmux
    entr
    xdotool
    xfce.thunar
    yt-dlp
    pavucontrol
    ranger
    syncplay
    ffmpeg
    simplescreenrecorder
    nix-tree

    hugo
    texlive.combined.scheme-medium
    mpv
    zsh
    firefox
    spotify
    krita
    docker-compose
    qbittorrent

    inotify-tools
    unar
    unzip
    p7zip

    shellcheck
    git
    graphviz
    gnuplot
    rustup
    nodejs
    # terminal utilities
    parallel
    rlwrap
    fd
    bat
    # delta
    # data wrangling
    ripgrep
    jq
    miller
    # net
    wget
    htop
    lsof
    # random
    tokei

    # formatters
    alejandra
    neovim
  ];

  services.openssh = {
    enable = true;
  };
}

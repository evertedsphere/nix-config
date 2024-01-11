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
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # neovim-nightly-overlay.overlays.default
      inputs.emacs-overlay.overlays.default
    ];

    config = {allowUnfree = true;};
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Paris";

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
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

  nix = {
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      substituters = [
        "https://cache.nixos.org/"
        "https://ghc-nix.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "ghc-nix.cachix.org-1:wI8l3tirheIpjRnr2OZh6YXXNdK2fVQeOI4SVz/X8nA="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      trusted-users = ["s" "root"];
    };
  };

  security = {
    polkit.enable = true;
    sudo.wheelNeedsPassword = false;
  };

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
    DefaultTimeoutStartSec=30s
    DefaultTimeoutStopSec=30s
  '';

  console = {
    earlySetup = true;
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };

  programs.zsh.enable = true;
  environment.pathsToLink = ["/share/zsh"];
  environment.shells = with pkgs; [bashInteractive zsh];
  users.defaultUserShell = pkgs.zsh;

  boot.kernelModules = ["uinput"];

  virtualisation.docker = {
    enable = true;
  };

  services.tumbler.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.nh.packages.${pkgs.hostPlatform.system}.default
    nix-output-monitor

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

    hugo
    texlive.combined.scheme-medium
    mpv
    zsh
    firefox
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
    # emacs-lsp
    (pkgs.emacsWithPackages (epkgs: [
      # for org-roam, which fails to function otherwise
      # also see note in packages.el
      epkgs.emacsql-sqlite
      epkgs.org-roam
    ]))
  ];

  services.openssh = {
    enable = true;
  };
}

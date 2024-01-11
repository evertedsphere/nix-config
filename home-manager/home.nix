{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: let
  renderTemplates = renderTemplate: data: let
    attrsToList = attrSet:
      lib.zipListsWith (name: value: {inherit name value;})
      (builtins.attrNames attrSet)
      (builtins.attrValues attrSet);
    f = arg: attrsToList (renderTemplate arg);
  in
    builtins.listToAttrs (builtins.concatMap f data);
  c = config.colorScheme.colors;
  h = x: "#${x}";
in {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    outputs.homeManagerModules.local
    ./xsession.nix
    ./alacritty.nix
    ./neovim.nix
    ./i3status-rs.nix
  ];

  home = {
    username = config.local.user.localUser;
    homeDirectory = "/home/${config.local.user.localUser}";
  };

  programs.home-manager.enable = true;
  news.display = "show";
  home.enableNixpkgsReleaseCheck = true;
  home.extraOutputsToInstall = ["doc" "info" "devdoc"];
  systemd.user.startServices = "sd-switch";

  programs.wofi.enable = true;
  programs.qutebrowser = {
    enable = true;
  };

  # services.gammastep = {
  #   enable = true;
  #   provider = "geoclue2";
  #   temperature = {
  #     day = 6000;
  #     night = 4600;
  #   };
  #   settings = {
  #     #general.adjustment-method = "wayland";
  #   };
  # };

  programs.atuin = {
    enable = true;
  };

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 30;
    gtk.enable = true;
    x11.enable = true;
  };

  programs.firefox = {
    enable = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };

  home.packages = with pkgs; [
    (pkgs.writeShellApplication {
      name = "bqn-alacritty";
      runtimeInputs = [];
      text = ''
        # FIXME fail unless alacritty
        ${config.local.programs.terminalExe} -o font.normal.family='"BQN386 Unicode"'
      '';
    })
    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "org-protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeTypes = ["x-scheme-handler/org-protocol"];
    })
  ];

  # misc
  xdg.enable = true;
  xdg.mimeApps = {
    enable = true;
    defaultApplications = let
      videoPlayer = "mpv.desktop";
    in {
      # FIXME mpv.installed ?
      "video/mp4" = videoPlayer;
      "video/x-matroska" = videoPlayer;
      "x-www-browser" = "Firefox.desktop";
      "inode/directory" = "thunar.desktop";
    };
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    delta.enable = true;
    lfs.enable = true;
    ignores = [".direnv/" "result/"];
    extraConfig = {
      user.name = config.local.user.fullName;
      user.email = config.local.user.email;
    };
  };

  programs.info.enable = true;
  programs.htop = {
    enable = true;
    settings = {
      hide_threads = true;
      hide_userland_threads = true;
      highlight_base_name = true;
      show_program_path = false;
      tree_view = true;
    };
  };
  programs.broot = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "fd --type f";
  };
  programs.command-not-found.enable = true;
  programs.readline.enable = true;
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    terminal = config.local.programs.terminalExe;
    theme = "sidebar";
    font = "${config.local.fonts.monospaceFont} 22";
  };

  programs.zsh = {
    enable = true;

    history.save = 100000;
    history.size = 100000;

    initExtra = ''
      path=("$HOME/.local/bin" "$HOME/.config/emacs/bin" "$HOME/.cargo/bin" "$HOME/.cabal/bin" $path)
      export PATH
    '';

    initExtraBeforeCompInit = ''
      # fpath=("$HOME/.zfunc" $fpath)
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/site-functions"
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/$ZSH_VERSION/functions"
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/vendor-completions"
    '';

    prezto = {
      enable = true;
      ssh = {identities = ["id_rsa" "id_ed25519"];};
      prompt.theme = "pure";
      pmodules = [
        # "environment"
        # "terminal"
        "editor"
        # "history"
        # "directory"
        # "syntax-highlighting"
        # "autosuggestions"
        # "archive"
        # "spectrum"
        # "utility"
        "ssh"
        # "git"
        # "docker"
        # "completion"
        # "fasd"
        "prompt"
      ];
    };
  };

  # set a random background
  home.file.wallpaper = {
    target = ".local/share/walls/wall.jpg";
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/.local/share/walls";
    display = "fill";
    # only set on login
    interval = null;
    enableXinerama = true;
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    documents = "docs";
    download = "inbox";
    pictures = "img";
    videos = "media";
    music = "media";
    desktop = ".desktop";
    publicShare = ".public";
    templates = ".templates";
  };

  home.file.firefox-userchrome = {
    source = ./config/userChrome.css;
    target = ".mozilla/firefox/default/chrome/userChrome.css";
  };

  # gpg
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gnome3";
  };

  services.udiskie = {
    enable = false;
    notify = true;
    tray = "auto";
    automount = true;
  };

  services.flameshot = {enable = true;};
  services.fluidsynth.enable = true;

  programs.zathura = {
    enable = true;
    options = {
      window-title-basename = true;
    };
    mappings = {
      "space" = "navigate next";
    };
  };

  programs.dircolors.enable = true;
  fonts.fontconfig.enable = true;
}

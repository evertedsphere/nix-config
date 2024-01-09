{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}:
let

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
    # ./hyprland.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      inputs.emacs-overlay.overlays.default
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = config.local.user.localUser;
    homeDirectory = "/home/${config.local.user.localUser}";
  };

  home.stateVersion = "22.11";
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

  programs.home-manager.enable = true;

  # zfs bug
  # programs.atuin.enable = true;

  programs.rtorrent = {
    enable = true;
    extraConfig = ''
    '';
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
    discord
    (pkgs.writeShellApplication {
      name = "bqn-alacritty";
      runtimeInputs = [];
      text = ''
        # FIXME fail unless alacritty
        ${config.local.programs.terminalExe} -o font.normal.family="BQN386 Unicode"
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
      ssh = {identities = ["id_rsa"];};
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

  # services.dunst = { enable = true; };
  services.flameshot = {enable = true;};
  services.fluidsynth.enable = true;
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -i ${config.home.file.wallpaper.target}";
  };

  # x
  services.picom = {
    enable = true;
    backend = "xrender";
    fade = true;
    vSync = true;
    fadeDelta = 3;
    fadeSteps = [0.04 0.04];
    # inactiveDim = "0.10";
  };

  programs.zathura = {
    enable = true;
    options = {
      window-title-basename = true;
    };
    mappings = {
      "space" = "navigate next";
    };
  };

  qt.style = {
    package = pkgs.adwaita-qt;
    name = "adwaita-dark";
  };

  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Paper";
      package = pkgs.paper-icon-theme;
    };
  };

  programs.dircolors.enable = true;

  fonts.fontconfig.enable = true;

  news.display = "show";
  services.dunst = {
    enable = true;
    settings = {
      global = {
        alignment = "center";
        bounce_freq = 0;
        corner_radius = 6;
        font = config.local.fonts.monospaceFont;
        format = "<b>%s</b>\\n%b";
        geometry = "350x5-25+25";
        horizontal_padding = 8;
        idle_threshold = 120;
        ignore_newline = false;
        indicate_hidden = true;
        line_height = 0;
        markup = "full";
        max_icon_size = 60;
        padding = 8;
        separator_color = "#00000000";
        separator_height = 5;
        show_age_threshold = 30;
        sort = true;
        startup_notification = false;
        sticky_history = true;
        transparency = 0;
        word_wrap = true;
      };
    };
  };
}

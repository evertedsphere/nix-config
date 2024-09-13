{
  inputs,
  outputs,
  config,
  pkgs,
  ...
}: let
  c = config.colorScheme.palette;
  h = x: "#${x}";
  colour_with_transparency = tr: x: "#${x}${tr}";
  ht = colour_with_transparency config.local.opacityHex;
in {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    inputs.nix-doom-emacs-unstraightened.hmModule
    inputs.wired-notify.homeManagerModules.default
    # inputs.stylix.homeManagerModules.stylix
    outputs.homeManagerModules.local
    outputs.homeManagerModules.keyd-application-mapper
    outputs.homeManagerModules.org-protocol
    ./i3.nix
    ./alacritty.nix
    ./neovim.nix
    ./i3status-rs.nix
    ./wired.nix
    ./stylix.nix
    ./emacs.nix
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
  xsession.enable = true;
  services.blueman-applet.enable = true;

  programs.wofi.enable = true;

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

  services.kdeconnect = {
    enable = true;
  };

  programs.atuin = {
    enable = true;
  };

  home.pointerCursor = {
    # package = pkgs.bibata-cursors;
    # name = "Bibata-Modern-Classic";
    # size = 30;
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

  # services.keyd-application-mapper = {
  #   enable = true;
  #   extraOptions = ["-v"];
  #   settings = {
  #     discord = {
  #       # list navigation
  #       "alt.n" = "down";
  #       "alt.p" = "up";
  #       # channel navigation
  #       "alt.j" = "A-down";
  #       "alt.k" = "A-up";
  #       "alt.shift.j" = "A-S-down";
  #       "alt.shift.k" = "A-S-up";
  #       # server navigation
  #       "alt.h" = "C-S-tab";
  #       "alt.l" = "C-tab";
  #       # text editing
  #       "ctrl.w" = "C-backspace";
  #     };
  #   };
  # };

  home.packages = with pkgs; [
    (pkgs.writeShellApplication {
      name = "bqn-alacritty";
      runtimeInputs = [];
      text = ''
        # FIXME fail unless alacritty
        ${config.local.programs.terminalExe} -o font.normal.family='"BQN386 Unicode"'
      '';
    })
    (pkgs.writeShellApplication {
      name = "systemd-user-unit-toggle";
      text = ''
        if systemctl --user is-active --quiet "$1";
          then systemctl --user stop "$1";
          else systemctl --user start "$1";
        fi
      '';
    })
    (pkgs.writeShellApplication {
      name = "open-pdf";
      text = ''
        cd ~/o/kb/refs
        pdf_to_open="$(fd . . --extension=pdf | rofi -dmenu)"
        if [ -n "$pdf_to_open" ]; then zathura "$pdf_to_open"; fi
      '';
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

  systemd.user.services.screenkey = let
    screenkey-script = pkgs.resholve.writeScript "run-screenkey" {
      inputs = with pkgs; [xorg.xrandr gnused gnugrep coreutils screenkey i3 jq];
      execer = [
        "cannot:${pkgs.screenkey}/bin/screenkey"
      ];
      interpreter = "${pkgs.bash}/bin/bash";
    } (builtins.readFile ./run-screenkey.sh);
  in {
    Unit = {
      Description = "Run a screenkey instance";
    };
    Service = {
      ExecStart = screenkey-script;
    };
  };

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    # TODO fix delta
    delta.enable = false;
    lfs.enable = true;
    ignores = [".direnv/" "result/"];
    extraConfig = {
      user.name = config.local.user.fullName;
      user.email = config.local.user.email;
    };
  };

  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-mozc
      fcitx5-gtk
      fcitx5-rime
      fcitx5-table-extra
      fcitx5-table-other
      # themes
      fcitx5-nord
      fcitx5-rose-pine
    ];
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
    font = "${config.stylix.fonts.monospace.name} 22";
  };

  programs.helix = {
    enable = true;
  };

  programs.zoxide = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    defaultKeymap = "emacs";
    autocd = true;

    history.save = 100000;
    history.size = 100000;
    # initExtra = builtins.readFile ./zsh-init.zsh;
    shellAliases = {
      "cd" = "z";
    };
    shellGlobalAliases = {
      "@w" = "wc -l";
      ":w" = "| @w";
      ":l" = "| less";
      "@xo" = "xclip -sel clip -o";
      "@xi" = "xclip -sel clip -i";
      ":xi" = "| @xi";
      ":x" = "| xargs";
      ":j" = "| jq";
      ":jc" = ":j -c";
      ":jcr" = ":j -cr";
      ":g" = "| grep";
      ":gv" = "| grep -v";
      ":sd" = "| sed-delete";
    };

    antidote = {
      enable = true;
      useFriendlyNames = true;
      plugins = [
        "sindresorhus/pure"
        "peterhurford/up.zsh"
        "zsh-users/zsh-autosuggestions"
        "zsh-users/zsh-history-substring-search"
      ];
    };

    initExtraFirst = ''
      if [ -n "''${ZSH_DEBUGRC+1}" ]; then
          zmodload zsh/zprof
      fi

      export PURE_PROMPT_SYMBOL="$"
      export PURE_PROMPT_VICMD_SYMBOL=":"
      export PURE_CMD_MAX_EXEC_TIME="1"
    '';

    initExtra = ''
      if [ -n "''${ZSH_DEBUGRC+1}" ]; then
          zprof
      fi

      nsh () {
        nix-shell --run zsh -p "$@"
      }

      sed-delete () {
        sed "s/$1//g"
      }
    '';

    initExtraBeforeCompInit = ''
      # fpath=("$HOME/.zfunc" $fpath)
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/site-functions"
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/$ZSH_VERSION/functions"
      # fpath+="/etc/profiles/per-user/$USER/share/zsh/vendor-completions"
    '';
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
    # gnome3 does not work
    pinentryPackage = pkgs.pinentry-qt;
  };

  services.udiskie = {
    enable = false;
    notify = true;
    tray = "auto";
    automount = true;
  };

  services.flameshot = {enable = true;};

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

  services.syncthing = {
    enable = true;
    tray = {
      enable = true;
    };
  };
}

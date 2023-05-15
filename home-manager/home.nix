{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}:

{
  imports =
    (with inputs; [
      nix-colors.homeManagerModules.default
    ])
    ++ (with outputs.homeManagerModules; [
      local
    ])
    ++ [ ./xsession.nix ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      # neovim-nightly-overlay.overlays.default
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "s";
    homeDirectory = "/home/s";
  };

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

  programs.alacritty = {
    enable = true;
    settings = {
      colors = let
        colors = config.colorScheme.colors;
      in {
        primary = {
          background = "#${colors.base00}";
          foreground = "#${colors.base05}";
        };
        cursor = {
          text = "#${colors.base00}";
          cursor = "#${colors.base05}";
        };
        normal = {
          black = "#${colors.base00}";
          red = "#${colors.base08}";
          green = "#${colors.base0B}";
          yellow = "#${colors.base0A}";
          blue = "#${colors.base0D}";
          magenta = "#${colors.base0E}";
          cyan = "#${colors.base0C}";
          white = "#${colors.base05}";
        };
        bright = {
          black = "#${colors.base03}";
          red = "#${colors.base08}";
          green = "#${colors.base0B}";
          yellow = "#${colors.base0A}";
          blue = "#${colors.base0D}";
          magenta = "#${colors.base0E}";
          cyan = "#${colors.base0C}";
          white = "#${colors.base07}";
        };
        indexed_colors = [
          {
            index = 16;
            color = "#${colors.base09}";
          }
          {
            index = 17;
            color = "#${colors.base0F}";
          }
          {
            index = 18;
            color = "#${colors.base01}";
          }
          {
            index = 19;
            color = "#${colors.base02}";
          }
          {
            index = 20;
            color = "#${colors.base04}";
          }
          {
            index = 21;
            color = "#${colors.base06}";
          }
        ];
      };
      window = {
        opacity = 0.93;
        dimensions = {
          columns = 100;
          lines = 85;
        };
        padding = {
          # i3 gap width
          x = 12;
          y = 12;
        };
        dynamic_padding = false;
        decorations = "None";
        startup_mode = "Windowed";
      };

      scrolling = {
        history = 10000;
        multiplier = 3;
      };

      font = {
        normal = {
          family = config.local.fonts.monospaceFont;
        };
        size = 12.0;
      };
    };
  };

  programs.home-manager.enable = true;
  # programs.atuin.enable = true;

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  programs.firefox = {
    enable = true;
  };

  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 32;
        output = [
          "DP-2"
          "HDMI-A-2"
        ];
        modules-left = ["wlr/workspaces" "wlr/taskbar"];
        modules-center = [];
        modules-right = ["temperature"];

        "wlr/workspaces" = {
          disable-scroll = true;
          all-outputs = false;
        };
      };
    };
  };

  systemd.user.startServices = "sd-switch";

  home.stateVersion = "22.11";

  home = {
    enableNixpkgsReleaseCheck = true;
    extraOutputsToInstall = ["doc" "info" "devdoc"];
    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
    packages = with pkgs; [
      discord
      (pkgs.writeShellApplication {
        name = "bqn-alacritty";
        runtimeInputs = [];
        text = ''
          ${pkgs.alacritty}/bin/alacritty -o font.normal.family="BQN386 Unicode"
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
  };

  # misc
  xdg.mimeApps = {
    enable = true;
    defaultApplications = let
      videoPlayer = "mpv.desktop";
    in {
      # FIXME mpv.installed ?
      "video/mp4" = videoPlayer;
      "video/x-matroska" = videoPlayer;
      "x-www-browser" = "Firefox.desktop";
    };
  };
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    delta.enable = true;
    lfs.enable = true;
    ignores = [".direnv/" ".envrc" "result/"];
    extraConfig = {
      user.name = "Soham Chowdhury";
      user.email = "evertedsphere@gmail.com";
      safe.directory = "/etc/nixos";
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
    nix-direnv = {enable = true;};
  };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = "sidebar";
    font = "${config.local.fonts.monospaceFont} 22";
  };

  programs.zsh = {
    enable = true;

    history.save = 100000;
    history.size = 100000;

    initExtra = ''
      path=("$HOME/.local/bin" "$HOME/.cargo/bin" "$HOME/.cabal/bin" $path)
      fpath=("$HOME/.zfunc" $fpath)
      export PATH

      # kitty shell integration
      # export KITTY_SHELL_INTEGRATION="enabled"
      # autoload -Uz -- ${pkgs.kitty}/lib/kitty/shell-integration/zsh/kitty-integration
      # kitty-integration
      # unfunction kitty-integration
    '';

    initExtraBeforeCompInit = ''
      fpath+="/etc/profiles/per-user/$USER/share/zsh/site-functions"
      fpath+="/etc/profiles/per-user/$USER/share/zsh/$ZSH_VERSION/functions"
      fpath+="/etc/profiles/per-user/$USER/share/zsh/vendor-completions"
    '';

    prezto = {
      enable = true;
      ssh = {identities = ["id_github_ed25519"];};
      prompt.theme = "pure";
      pmodules = [
        "environment"
        "terminal"
        "editor"
        "history"
        "directory"
        "spectrum"
        "syntax-highlighting"
        "autosuggestions"
        "archive"
        "spectrum"
        "utility"
        "ssh"
        "git"
        "docker"
        "completion"
        "fasd"
        "prompt"
      ];
    };
  };

  # set a random background
  home.file.wallpaper = {
    source = ../seto_miyako.jpg;
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
    pinentryFlavor = "gtk2";
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
  services.picom = let
    shadowRadius = 15;
    shadowOffset = -1 * shadowRadius;
  in {
    enable = true;
    backend = "glx";
    settings = {
      # blur = {
      #   method = "dual_kawase";
      #   strength = 8;
      #   background = false;
      #   background-frame = false;
      #   background-fixed = false;
      # };
      shadow-radius = shadowRadius;
      # (disabled) slightly rounded corners
      # corner-radius = 10.0;
      no-dnd-shadow = true;
      no-dock-shadow = true;
    };
    fade = true;
    vSync = true;

    shadow = false;
    shadowOpacity = 0.6;
    shadowOffsets = [shadowOffset shadowOffset];

    fadeDelta = 3;
    fadeSteps = [0.04 0.04];
    # inactiveDim = "0.10";
  };

  programs.zathura = {
    enable = true;
    options = {
      window-title-basename = true;
    };
  };
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      blocks = [
        {block = "cpu";}
        {
          block = "disk_space";
          path = "/";
          info_type = "available";
          interval = 20;
          warning = 20.0;
          alert = 10.0;
          format = " $icon root: $available.eng(w:2) ";
        }
        {
          block = "memory";
          format = " $icon $mem_total_used_percents.eng(w:2) ";
          format_alt = " $icon_swap $swap_used_percents.eng(w:2) ";
        }
        {
          block = "sound";
          click = [
            {
              button = "left";
              cmd = "pavucontrol";
            }
          ];
        }
        {
          block = "time";
          interval = 5;
          format = " $timestamp.datetime(f:'%a %d/%m %R') ";
        }
      ];
    };
  };

  programs.neovim = {
    enable = true;
    extraConfig = ''
      set nocompatible

      filetype plugin indent on
      syntax on

      "https://vim.fandom.com/wiki/Example_vimrc
      set hidden
      set wildmenu
      set showcmd
      set hlsearch
      set nomodeline
      set ignorecase
      set smartcase
      set backspace=indent,eol,start
      set autoindent
      "set nostartofline
      set ruler
      set laststatus=2
      set confirm
      set visualbell
      set t_vb=
      set mouse=a
      set cmdheight=2
      set nonumber
      set notimeout ttimeout ttimeoutlen=200
      set pastetoggle=<F11>
      set shiftwidth=2
      set softtabstop=2
      set expandtab
      set clipboard=unnamedplus
      set undofile
      set nobackup
      set nowritebackup
      set updatetime=300
      set shortmess+=c
      set signcolumn=yes
      set dir=~/.swp
      set list
      set listchars=nbsp:¬,tab:→\ ,extends:»,precedes:«,trail:·,space:·

      packloadall

      set background=dark
      set termguicolors
      let ayucolor="dark"
      colorscheme ayu

      hi Normal     ctermbg=NONE guibg=NONE
      hi LineNr     ctermbg=NONE guibg=NONE
      hi SignColumn ctermbg=NONE guibg=NONE
    '';
    plugins = with pkgs.vimPlugins; [
      ayu-vim
      vim-commentary
      vim-surround
      vim-nix
      colorizer
    ];
  };

  # qt.style = {
  #   package = pkgs.adwaita-qt;
  #   name = "adwaita-dark";
  # };
  # gtk = {
  #   enable = true;
  #   theme = {
  #     name = "Dracula";
  #     package = pkgs.dracula-theme;
  #   };
  #   iconTheme = {
  #     name = "Paper";
  #     package = pkgs.paper-icon-theme;
  #   };
  # };

  programs.dircolors.enable = true;
  fonts.fontconfig.enable = true;

  # manual = {
  #   html.enable = true;
  #   manpages.enable = true;
  #   json.enable = true;
  # };

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
      urgency_low = {
        background = "${color2}";
        foreground = "${background}";
        timeout = 5;
      };
      urgency_normal = {
        background = "${color3}";
        foreground = "${background}";
        timeout = 20;
      };
      urgency_critical = {
        background = "${color1}";
        foreground = "${background}";
        timeout = 0;
      };
    };
  };
}

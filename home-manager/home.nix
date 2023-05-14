{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  colors = config.colorScheme.colors;

  foreground = "#979eab";
  background = "#1f2329";
  selection_foreground = "#5b6268";
  selection_background = "#b3deef";

  url_color = "#61afef";

  color0 = "#282c34";
  color8 = "#393e48";

  # White
  color7 = "#979eab";
  color15 = "#bbc2cf";

  # Red
  color1 = "#e55561";
  color9 = "#8b3434";

  # Green
  color2 = "#8ebd6b";
  color10 = "#5e8d6b";

  # Blue
  color4 = "#4fa6ed";
  color12 = "#0f66ad";

  # Yellow
  color3 = "#e2b86b";
  color11 = "#835d1a";

  # Magenta
  color5 = "#bf68d9";
  color13 = "#7e3992";

  # Cyan
  color6 = "#48b0bd";
  color14 = "#266269";

  # Cursor colors
  cursor = "#cccccc";

  # Tab bar colors
  active_tab_foreground = "#282a36";
  active_tab_background = "#f8f8f2";
  inactive_tab_foreground = "#282a36";
  inactive_tab_background = "#6272a4";

  # Marks
  mark1_foreground = "#282a36";
  mark1_background = "#ff5555";

  gruvbox = {
    bg = "#282828";
    # bg = "#1d2021";
    red = "#cc241d";
    white = "#ffffff";
    green = "#98971a";
    yellow = "#d79921";
    blue = "#458588";
    purple = "#b16286";
    aqua = "#689d68";
    gray = "#a89984";
    darkgray = "#1d2021";
    lightgray = "#bdae93";
  };

  renderTemplates = renderTemplate: data: let
    attrsToList = attrSet:
      lib.zipListsWith (name: value: {inherit name value;})
      (builtins.attrNames attrSet)
      (builtins.attrValues attrSet);
    f = arg: attrsToList (renderTemplate arg);
  in
    builtins.listToAttrs (builtins.concatMap f data);
in {
  imports = [
    # outputs.homeManagerModules.example
    #./hyprland.nix
    inputs.nix-colors.homeManagerModules.default
  ];

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;

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

  services.gammastep = {
    enable = true;
    provider = "geoclue2";
    temperature = {
      day = 6000;
      night = 4600;
    };
    settings = {
      #general.adjustment-method = "wayland";
    };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      colors = {
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
        normal = {family = "Iosevka";};
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
    packages = [
      (pkgs.makeDesktopItem {
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
    font = "Iosevka 22";
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
    display = fill;
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
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -i ${config.home.file.wallpaper-folder.target}";
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
  xsession = {
    enable = true;
    initExtra = "";
    scriptPath = ".hm-xsession";
    numlock.enable = true;
    windowManager.i3 = {
      enable = true;
      config = let
        modifier = "Mod4";
        # FIXME
        movementMap = {
          left = "h";
          right = "l";
          up = "k";
          down = "j";
        };
        movementKeys = [
          {
            key = movementMap.left;
            dir = "left";
          }
          {
            key = movementMap.down;
            dir = "down";
          }
          {
            key = movementMap.up;
            dir = "up";
          }
          {
            key = movementMap.right;
            dir = "right";
          }
        ];
        mkWs = x: y: {
          wsName = "${x}:${y}";
          wsKey = y;
        };
        smallMonitorWss =
          map (x: mkWs x x) ["1" "2" "3" "4" "5" "6" "7" "8" "9" "0"];
        bigMonitorWss = [
          (mkWs "10" "q")
          (mkWs "11" "w")
          (mkWs "12" "e")
          (mkWs "13" "r")
          (mkWs "14" "t")
          (mkWs "15" "y")
          (mkWs "16" "u")
          (mkWs "17" "i")
        ];
        workspaceKeybinds =
          renderTemplates
          ({
            wsName,
            wsKey,
          }: {
            "${modifier}+${wsKey}" = "workspace number ${wsName}";
            "${modifier}+Shift+${wsKey}" = "move container to workspace number ${wsName}";
          })
          (smallMonitorWss ++ bigMonitorWss);
        containerKeybinds =
          renderTemplates
          ({
            key,
            dir,
          }: {
            "${modifier}+${key}" = "focus ${dir}";
            "${modifier}+Shift+${key}" = "move ${dir}";
          })
          movementKeys;
        assignWorkspace = outputs: let
          output = lib.concatStringsSep " " outputs;
        in
          map (workspace: {
            inherit output;
            workspace = workspace.wsName;
          });
        workspaceOutputAssign =
          # TODO primary/secondary outputs
          assignWorkspace ["eDP-1" "HDMI-A-0" "HDMI-1"] smallMonitorWss
          ++ assignWorkspace ["DP-2" "DisplayPort-0"] bigMonitorWss;
        run = x: "exec ${x}";
        spawn = x: "exec --no-startup-id ${x}";
        globalKeybinds = {
          "F8" = spawn "dm-tool switch-to-greeter";
          "Print" = spawn "flameshot screen -p ~/img/caps";
          "Shift+Print" = spawn "flameshot gui";
          "Ctrl+Print" = spawn "sleep 5 && flameshot gui";
          "${modifier}+Return" = run "${pkgs.alacritty}/bin/alacritty";
          "${modifier}+p" = run "${pkgs.rofi}/bin/rofi -modi drun -show drun";
        };
        controlKeybinds = {
          "${modifier}+Ctrl+q" = "kill";
          "${modifier}+b" = "split horizontal";
          "${modifier}+v" = "split vertical";
          "${modifier}+f" = "fullscreen toggle";
          "${modifier}+m" = "layout tabbed";
          "${modifier}+n" = "layout toggle split";
          "${modifier}+Ctrl+space" = "floating toggle";
          "${modifier}+space" = "focus mode_toggle";
          "${modifier}+a" = "focus parent";
          "${modifier}+d" = "focus child";
          "${modifier}+Ctrl+c" = "reload";
          "${modifier}+Ctrl+r" = "restart";
          "${modifier}+o" = "mode resize";
          "${modifier}+F10" = spawn "~/.local/bin/open-pdf";
        };
        colors = {
          # Background color of the window. Only applications which do not cover
          # the whole area expose the color.
          background = gruvbox.bg;

          # A window which currently has the focus
          focused = {
            border = gruvbox.yellow;
            background = gruvbox.yellow;
            text = gruvbox.darkgray;
            # text = gruvbox.yellow;
            indicator = gruvbox.lightgray;
            childBorder = gruvbox.yellow;
          };

          # A window which is the focused one of its container,
          # but it does not have the focus at the moment.
          focusedInactive = {
            border = gruvbox.lightgray;
            background = gruvbox.lightgray;
            text = gruvbox.darkgray;
            # hide indicator
            indicator = gruvbox.lightgray;
            childBorder = gruvbox.lightgray;
          };

          # A window which is not focused
          unfocused = {
            border = gruvbox.bg;
            background = gruvbox.bg;
            text = gruvbox.lightgray;
            # text = gruvbox.bg;
            indicator = gruvbox.bg;
            childBorder = gruvbox.bg;
          };

          # A window which has its urgency hint activated.
          urgent = {
            border = gruvbox.red;
            background = gruvbox.red;
            text = gruvbox.white;
            # text = gruvbox.red;
            indicator = gruvbox.red;
            childBorder = gruvbox.red;
          };

          # Background and text color are used to draw placeholder window
          # contents (when restoring layouts). Border and indicator are ignored.
          placeholder = {
            border = gruvbox.darkgray;
            background = gruvbox.darkgray;
            text = gruvbox.yellow;
            indicator = gruvbox.lightgray;
            childBorder = gruvbox.darkgray;
          };
        };
        # FIXME
      in
        lib.mkOptionDefault {
          inherit modifier colors;
          terminal = "${pkgs.alacritty}/bin/alacritty";
          fonts = {
            names = ["Iosevka Nerd Font"];
            size = 11.0;
          };
          workspaceAutoBackAndForth = true;
          gaps = {
            inner = 14;
            outer = 0;
          };
          floating = {
            inherit modifier;
            titlebar = false;
          };
          assigns = {
            # "2:2" = [{ class = "^firefox$"; }];
            "5:5" = [{class = "^Spotify$";}];
            "7:7" = [{class = "^Discord$";} {class = "^Dragon";}];
            "9:9" = [{class = "^qBittorrent$";}];
          };
          window = let
            borderWidth = 2;
          in {
            border = borderWidth;
            commands = [
              {
                command = "border pixel ${builtins.toString borderWidth}";
                criteria = {class = ".*";};
              }
            ];
          };
          modes = {
            resize = {
              # TODO resize grow right etc
              "${movementMap.down}" = "resize grow height 50 px or 10 ppt";
              "${movementMap.left}" = "resize shrink width 50 px or 10 ppt";
              "${movementMap.right}" = "resize grow width 50 px or 10 ppt";
              "${movementMap.up}" = "resize shrink height 50 px or 10 ppt";
              Escape = "mode default";
            };
          };
          keybindings =
            containerKeybinds
            // workspaceKeybinds
            // globalKeybinds
            // controlKeybinds;
          inherit workspaceOutputAssign;
          bars = [
            {
              mode = "dock";

              hiddenState = "hide";
              position = "top";
              workspaceButtons = true;
              workspaceNumbers = false;
              # TODO don't hardcode
              statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml";
              fonts = {
                names = ["Iosevka Nerd Font"];
                size = 12.0;
              };
              trayOutput = "DP-2";
              colors = {
                background = gruvbox.bg;
                statusline = gruvbox.lightgray;
                separator = gruvbox.lightgray;
                focusedWorkspace = {
                  border = gruvbox.yellow;
                  background = gruvbox.yellow;
                  text = gruvbox.darkgray;
                };
                inactiveWorkspace = {
                  border = gruvbox.bg;
                  background = gruvbox.bg;
                  text = gruvbox.lightgray;
                };
                activeWorkspace = {
                  border = gruvbox.lightgray;
                  background = gruvbox.lightgray;
                  text = gruvbox.darkgray;
                };
                bindingMode = {
                  border = gruvbox.red;
                  background = gruvbox.red;
                  text = gruvbox.bg;
                };
                urgentWorkspace = {
                  border = gruvbox.red;
                  background = gruvbox.red;
                  text = gruvbox.bg;
                };
              };
            }
          ];
        };
    };
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
        font = "Iosevka Nerd Font";
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

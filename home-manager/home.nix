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
    ./hyprland.nix
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

  wayland.windowManager.sway = {
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
          map (x: mkWs x x) ["9" "0"];
        leftMonitorWss =
          map (x: mkWs x x) ["1" "2" "3" "4" "5" "6" "7" "8"];
        rightMonitorWss = [
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
          (smallMonitorWss ++ leftMonitorWss ++ rightMonitorWss);
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
          assignWorkspace ["HDMI-A-1"] smallMonitorWss
          ++ assignWorkspace ["HDMI-A-2"] leftMonitorWss
          ++ assignWorkspace ["DP-2"] rightMonitorWss;
        run = x: "exec ${x}";
        spawn = x: "exec --no-startup-id ${x}";
        globalKeybinds = {
          # "F8" = spawn "dm-tool switch-to-greeter";
          "Print" = spawn "flameshot screen -p ~/img/caps";
          "Shift+Print" = spawn "flameshot gui";
          "Ctrl+Print" = spawn "sleep 5 && flameshot gui";
          "${modifier}+Return" = run config.local.programs.terminalExe;
          "${modifier}+p" = run "${pkgs.rofi}/bin/rofi -modi drun -show drun";
        };
        controlKeybinds = {
          "${modifier}+Ctrl+q" = "kill";
          "${modifier}+Ctrl+space" = "floating toggle";
          "${modifier}+Ctrl+c" = "reload";
          "${modifier}+Ctrl+r" = "restart";
          "${modifier}+b" = "split horizontal";
          "${modifier}+v" = "split vertical";
          "${modifier}+f" = "fullscreen toggle";
          "${modifier}+m" = "layout tabbed";
          "${modifier}+n" = "layout toggle split";
          "${modifier}+space" = "focus toggle_mode";
          "${modifier}+a" = "focus parent";
          "${modifier}+d" = "focus child";
          "${modifier}+o" = "mode resize";
          "${modifier}+F10" = spawn "~/.local/bin/open-pdf";
        };
        colors = {
          # Background color of the window. Only applications which do not cover
          # the whole area expose the color.
          background = h c.base01;

          # A window which currently has the focus
          focused = {
            border = h c.base0A;
            background = h c.base0A;
            text = h c.base00;
            indicator = h c.base03;
            childBorder = h c.base0A;
          };

          # A window which is the focused one of its container,
          # but it does not have the focus at the moment.
          focusedInactive = {
            border = h c.base03;
            background = h c.base03;
            text = h c.base00;
            # hide indicator
            indicator = h c.base03;
            childBorder = h c.base03;
          };

          # A window which is not focused
          unfocused = {
            border = h c.base01;
            background = h c.base01;
            text = h c.base03;
            # text = h c.base01;
            indicator = h c.base01;
            childBorder = h c.base01;
          };

          # A window which has its urgency hint activated.
          urgent = {
            border = h c.base08;
            background = h c.base08;
            text = h c.base07;
            # text = h c.base08;
            indicator = h c.base08;
            childBorder = h c.base08;
          };

          # Background and text color are used to draw placeholder window
          # contents (when restoring layouts). Border and indicator are ignobase08.
          placeholder = {
            border = h c.base00;
            background = h c.base00;
            text = h c.base0A;
            indicator = h c.base03;
            childBorder = h c.base00;
          };
        };
        # FIXME
      in
        lib.mkOptionDefault {
          inherit modifier colors;
          terminal = config.local.programs.terminalExe;
          fonts = {
            names = [config.local.fonts.monospaceFont];
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
                names = [config.local.fonts.monospaceFont];
                size = 12.0;
              };
              # trayOutput = "DP-2";
              colors = {
                background = h c.base00;
                statusline = h c.base03;
                separator = h c.base03;
                focusedWorkspace = {
                  border = h c.base0A;
                  background = h c.base0A;
                  text = h c.base00;
                };
                inactiveWorkspace = {
                  border = h c.base00;
                  background = h c.base00;
                  text = h c.base03;
                };
                activeWorkspace = {
                  border = h c.base03;
                  background = h c.base03;
                  text = h c.base00;
                };
                bindingMode = {
                  border = h c.base08;
                  background = h c.base08;
                  text = h c.base00;
                };
                urgentWorkspace = {
                  border = h c.base08;
                  background = h c.base08;
                  text = h c.base00;
                };
              };
            }
          ];
        };
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      # needs qt5.qtwayland in systemPackages
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
      export WLR_NO_HARDWARE_CURSORS=1
#      export WLR_RENDERER=vulkan
      export XWAYLAND_NO_GLAMOR=1
    '';
    extraOptions = [
      "--verbose"
      "--debug"
      "--unsupported-gpu"
    ];
    extraConfig = ''
      output HDMI-A-2 scale 1.5
      output DP-2     scale 1.5
      output HDMI-A-1 scale 1.3

      output HDMI-A-2 pos 0    0    res 3840x2160
      output DP-2     pos 3840 0    res 3840x2160
      output HDMI-A-1 pos 3840 2160 res 2560x1440
    '';
  };
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
      fpath=("$HOME/.zfunc" $fpath)
      export PATH
    '';

    initExtraBeforeCompInit = ''
      fpath+="/etc/profiles/per-user/$USER/share/zsh/site-functions"
      fpath+="/etc/profiles/per-user/$USER/share/zsh/$ZSH_VERSION/functions"
      fpath+="/etc/profiles/per-user/$USER/share/zsh/vendor-completions"
    '';

    prezto = {
      enable = true;
      ssh = {identities = ["id_rsa"];};
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
    source = ../jellyfish.jpg;
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
  services.picom = {
    enable = false;
    backend = "glx";
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
      # urgency_low = {
      #   background = "${color2}";
      #   foreground = "${background}";
      #   timeout = 5;
      # };
      # urgency_normal = {
      #   background = "${color3}";
      #   foreground = "${background}";
      #   timeout = 20;
      # };
      # urgency_critical = {
      #   background = "${color1}";
      #   foreground = "${background}";
      #   timeout = 0;
      # };
    };
  };
}

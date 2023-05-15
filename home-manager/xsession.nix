{
  config,
  lib,
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
in {
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
        colors = let
          colors = config.colorScheme.colors;
          h = x: "#${x}";
        in {
          # Background color of the window. Only applications which do not cover
          # the whole area expose the color.
          background = h colors.base01;

          # A window which currently has the focus
          focused = {
            border = h colors.base0A;
            background = h colors.base0A;
            text = h colors.base00;
            indicator = h colors.base03;
            childBorder = h colors.base0A;
          };

          # A window which is the focused one of its container,
          # but it does not have the focus at the moment.
          focusedInactive = {
            border = h colors.base03;
            background = h colors.base03;
            text = h colors.base00;
            # hide indicator
            indicator = h colors.base03;
            childBorder = h colors.base03;
          };

          # A window which is not focused
          unfocused = {
            border = h colors.base01;
            background = h colors.base01;
            text = h colors.base03;
            # text = h colors.base01;
            indicator = h colors.base01;
            childBorder = h colors.base01;
          };

          # A window which has its urgency hint activated.
          urgent = {
            border = h colors.base08;
            background = h colors.base08;
            text = h colors.base07;
            # text = h colors.base08;
            indicator = h colors.base08;
            childBorder = h colors.base08;
          };

          # Background and text color are used to draw placeholder window
          # contents (when restoring layouts). Border and indicator are ignobase08.
          placeholder = {
            border = h colors.base00;
            background = h colors.base00;
            text = h colors.base0A;
            indicator = h colors.base03;
            childBorder = h colors.base00;
          };
        };
        # FIXME
      in
        lib.mkOptionDefault {
          inherit modifier colors;
          terminal = "${pkgs.alacritty}/bin/alacritty";
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
                names = [config.local.fonts.monospaceFont];
                size = 12.0;
              };
              trayOutput = "DP-2";
              colors = {
                background = colors.base01;
                statusline = colors.base03;
                separator = colors.base03;
                focusedWorkspace = {
                  border = colors.base0A;
                  background = colors.base0A;
                  text = colors.base00;
                };
                inactiveWorkspace = {
                  border = colors.base01;
                  background = colors.base01;
                  text = colors.base03;
                };
                activeWorkspace = {
                  border = colors.base03;
                  background = colors.base03;
                  text = colors.base00;
                };
                bindingMode = {
                  border = colors.base08;
                  background = colors.base08;
                  text = colors.base01;
                };
                urgentWorkspace = {
                  border = colors.base08;
                  background = colors.base08;
                  text = colors.base01;
                };
              };
            }
          ];
        };
    };
  };
}

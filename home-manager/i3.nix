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
  c = config.colorScheme.palette;
  h = x: "#${x}";
  colour_with_transparency = tr: x: "#${x}${tr}";
in {
  xsession = {
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
            "${modifier}+${key}" = "exec --no-startup-id ~/.local/bin/winnav ${dir}";
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
          assignWorkspace ["HDMI-0"] smallMonitorWss
          ++ assignWorkspace ["HDMI-1"] leftMonitorWss
          ++ assignWorkspace ["DP-2"] rightMonitorWss;
        run = x: "exec ${x}";
        spawn = x: "exec --no-startup-id ${x}";
        spawn-pactl = x: spawn "${pkgs.pulseaudio}/bin/pactl ${x}";
        globalKeybinds = {
          # "F8" = spawn "dm-tool switch-to-greeter";
          "Print" = spawn "flameshot screen -p ~/img/caps";
          "Shift+Print" = spawn "flameshot gui";
          "Ctrl+Print" = spawn "sleep 5 && flameshot gui";
          "${modifier}+Return" = run config.local.programs.terminalExe;
          "${modifier}+p" = run "${pkgs.rofi}/bin/rofi -modi drun -show drun";
          "XF86AudioRaiseVolume" = spawn-pactl "set-sink-volume @DEFAULT_SINK@ +10%";
          "XF86AudioLowerVolume" = spawn-pactl "set-sink-volume @DEFAULT_SINK@ -10%";
          "XF86AudioMute" = spawn-pactl "set-sink-mute @DEFAULT_SINK@ toggle";
          # TODO should really be MicMute
          "Ctrl+XF86AudioMute" = spawn-pactl "set-source-mute @DEFAULT_SOURCE@ toggle";
          "${modifier}+Ctrl+e" = run "emacsclient -cnq";
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
          "${modifier}+space" = "focus mode_toggle";
          "${modifier}+a" = "focus parent";
          "${modifier}+d" = "focus child";
          "${modifier}+o" = "mode resize";
          "${modifier}+g" = "mode leader";
        };
        colors = {
          # Background color of the window. Only applications which do not cover
          # the whole area expose the color.
          background = h c.base01;

          # A window which currently has the focus
          focused = rec {
            border = h c.base0A;
            background = h c.base0A;
            # text = h c.base00;
            text = border;
            indicator = h c.base03;
            childBorder = h c.base0A;
          };

          # A window which is the focused one of its container,
          # but it does not have the focus at the moment.
          focusedInactive = rec {
            border = h c.base03;
            background = h c.base03;
            # text = h c.base00;
            text = border;
            # hide indicator
            indicator = h c.base03;
            childBorder = h c.base03;
          };

          # A window which is not focused
          unfocused = rec {
            border = h c.base01;
            background = h c.base01;
            # text = h c.base03;
            text = border;
            # text = h c.base01;
            indicator = h c.base01;
            childBorder = h c.base01;
          };

          # A window which has its urgency hint activated.
          urgent = rec {
            border = h c.base08;
            background = h c.base08;
            # text = h c.base07;
            text = border;
            # text = h c.base08;
            indicator = h c.base08;
            childBorder = h c.base08;
          };

          # Background and text color are used to draw placeholder window
          # contents (when restoring layouts). Border and indicator are ignobase08.
          "placeholder" = rec {
            border = h c.base00;
            background = h c.base00;
            # text = h c.base0A;
            text = border;
            indicator = h c.base03;
            childBorder = h c.base00;
          };
        };
        # FIXME
      in {
        inherit modifier colors;
        terminal = config.local.programs.terminalExe;
        fonts = {
          names = [config.local.fonts.monospaceFont];
          size = 0.0;
        };
        workspaceAutoBackAndForth = true;
        focus = {
          newWindow = "urgent";
        };
        gaps = {
          inner = config.local.gapWidth;
          outer = 0;
        };
        floating = {
          inherit modifier;
          titlebar = false;
          criteria = [
            {title = "doom-capture";}
          ];
        };
        assigns = {
          "11:w" = [{class = "Zotero";}];
          "2:2" = [{class = "firefox";}];
          "5:5" = [{class = "Spotify";}];
          "7:7" = [{class = "discord";}];
          "9:9" = [{class = "qBittorrent";}];
        };
        window = let
          borderWidth = 0;
        in {
          border = borderWidth;
          commands = [
            {
              command = "border normal ${builtins.toString borderWidth}";
              criteria = {class = ".*";};
            }
          ];
        };
        modes = {
          leader = {
            "s" = spawn "systemd-user-unit-toggle screenkey.service, mode default";
            "p" = spawn "open-pdf, mode default";
            "m" = spawn "~/.local/bin/org-capture-now-playing, mode default";
            Escape = "mode default";
          };
          resize = let
            resizeDeltaPx = 50;
            resizeDeltaPpt = 10;
            go = action: dir: "i3-msg resize ${action} ${dir} ${builtins.toString resizeDeltaPx} px or ${builtins.toString resizeDeltaPpt} ppt";
            resizeCmd = growDir: shrinkDir: run "${go "grow" growDir} || ${go "shrink" shrinkDir}";
          in {
            "${movementMap.left}" = resizeCmd "left" "right";
            "${movementMap.right}" = resizeCmd "right" "left";
            "${movementMap.up}" = resizeCmd "up" "down";
            "${movementMap.down}" = resizeCmd "down" "up";
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
              size = config.local.fonts.i3barFontSize;
            };
            command = "i3bar --transparency";
            trayOutput = "HDMI-0";
            # separatorSymbol = " \ ";
            colors = let
              ht = colour_with_transparency config.local.opacityHex;
            in {
              background = ht c.base00;
              statusline = ht c.base00;
              separator = ht c.base00;
              focusedWorkspace = rec {
                border = background;
                background = ht c.base00;
                text = h c.base0A;
              };
              inactiveWorkspace = rec {
                border = background;
                background = ht c.base00;
                text = h c.base03;
              };
              activeWorkspace = rec {
                border = background;
                background = ht c.base00;
                text = h c.base04;
              };
              bindingMode = rec {
                border = background;
                background = ht c.base08;
                text = h c.base00;
              };
              urgentWorkspace = rec {
                border = background;
                background = ht c.base08;
                text = h c.base00;
              };
            };
          }
        ];
      };
    };
  };
}

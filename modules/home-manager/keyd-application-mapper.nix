{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.keyd-application-mapper;

  settingsFormat = pkgs.formats.ini {};

  settingsStr = concatStringsSep "\n" (mapAttrsToList
    (hotkey: command:
      optionalString (command != null) ''
        ${hotkey}
          ${command}
      '')
    cfg.settings);
in {
  imports = [
  ];

  options.services.keyd-application-mapper = {
    enable = mkEnableOption "simple X hotkey daemon";

    package = mkOption {
      type = types.package;
      default = pkgs.keyd;
      defaultText = "pkgs.keyd";
      description = "Package containing the {command}`keyd` and {command}`keyd-application-mapper` executables.";
    };

    extraOptions = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Command line arguments to invoke {command}`keyd-application-mapper` with.";
      example = literalExpression ''[ "--quiet" ]'';
    };

    settings = mkOption {
      inherit (settingsFormat) type;
      default = {};
      example = {
        main = {
          capslock = "overload(control, esc)";
          rightalt = "layer(rightalt)";
        };

        rightalt = {
          j = "down";
          k = "up";
          h = "left";
          l = "right";
        };
      };
      description = lib.mdDoc ''
        Configuration that is written to {file}`$XDG_CONFIG_HOME/keyd/app.conf`.
        Appropriate names can be used to write non-alpha keys, for example "equal" instead of "=" sign (see <https://github.com/NixOS/nixpkgs/issues/236622>).
        See <https://github.com/rvaiya/keyd> how to configure.
      '';
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      description = "Additional configuration to add.";
      example = literalExpression ''
        super + {_,shift +} {1-9,0}
          i3-msg {workspace,move container to workspace} {1-10}
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.keyd-application-mapper" pkgs
        lib.platforms.linux)
    ];

    home.packages = [cfg.package];

    xdg.configFile."keyd/app.conf".source =
      settingsFormat.generate "app.conf" cfg.settings;
    # concatStringsSep "\n" [ settingsStr cfg.extraConfig ];

    systemd.user.services = mkIf cfg.enable {
      keyd-application-mapper = {
        Unit = {
          Description = "Keyd application specific remapping daemon";
          Documentation = "man:keyd-application-mapper";
          PartOf = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          ExecStart = "${cfg.package}/bin/keyd-application-mapper ${toString cfg.extraOptions}";
          Restart = "on-failure";
          OOMPolicy = "continue";
        };

        Install = {WantedBy = ["graphical-session.target"];};
      };
    };
  };
}

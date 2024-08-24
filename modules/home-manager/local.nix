{
  config,
  lib,
  pkgs,
  ...
}: let
  strOpt = example: description:
    lib.mkOption {
      type = lib.types.str;
      inherit example description;
    };
in {
  options.local = {
    user.fullName = strOpt "John Doe" "home-manager user's full name";
    user.email = strOpt "user@example.com" "home-manager user's email";
    user.localUser = strOpt "ubuntu" "s";
    programs = {
      terminalExe = lib.mkOption {
        type = lib.types.str;
        example = "\$\{pkgs.alacritty\}/bin/alacritty";
        description = "Path to the terminal emulator to use";
      };
    };
    fonts = {
      monospaceFont = lib.mkOption {
        type = lib.types.str;
        example = "Iosevka Term";
        description = "Monospace font to use";
      };
      alacrittyFontSize = lib.mkOption {
        type = lib.types.float;
      };
      i3barFontSize = lib.mkOption {
        type = lib.types.float;
        example = 20.0;
        description = "Monospace font size for i3bar";
      };
    };
    gapWidth = lib.mkOption {
      type = lib.types.int;
    };
    opacityHex = lib.mkOption {
      type = lib.types.str;
    };
    # TODO lol
    opacityFloat = lib.mkOption {
      type = lib.types.float;
    };
    hasBattery = lib.mkOption {
      type = lib.types.bool;
      example = false;
      description = "Whether this device has a battery";
    };
  };
}

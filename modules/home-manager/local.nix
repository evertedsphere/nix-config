{ config, lib, pkgs, ... }:

{
  options.local.fonts = with lib; {
    defaultMonospaceFont = mkOption {
      type = types.str;
      example = "Iosevka Term";
      description = "Monospace font to use";
    };
  };
}

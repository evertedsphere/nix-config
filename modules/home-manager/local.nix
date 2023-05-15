{
  config,
  lib,
  pkgs,
  ...
}: {
  options.local = {
    fonts = {
      monospaceFont = lib.mkOption {
        type = lib.types.str;
        example = "Iosevka Term";
        description = "Monospace font to use";
      };
    };
  };
}

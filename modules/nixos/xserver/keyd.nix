{
  config,
  lib,
  pkgs,
  ...
}: {
  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = ["*"];
      settings = {
        main = {
          capslock = "overload(control, esc)";
          tab = "overload(meta, tab)";
        };
      };
    };
  };
}

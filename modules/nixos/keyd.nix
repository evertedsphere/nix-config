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
          # This trick prevents random `esc`s in situations where that's undesirable.
          capslock = "timeout(overload(control, esc), 500, layer(control))";
          # Might as well do it for the other thing.
          tab = "timeout(overload(meta, tab), 500, layer(meta))";
        };
      };
    };
  };
}

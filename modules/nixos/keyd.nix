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
        main = let
          idleTimeout = 130;
          holdTimeout = 170;
          overloadTimeout = 200;
          # infuriatingly…unsuited to my usage
          # lettermod = layer: key: "lettermod(${layer}, ${key}, ${builtins.toString idleTimeout}, ${builtins.toString holdTimeout})";
          lettermod = layer: key: "overloadt(${layer}, ${key}, ${builtins.toString overloadTimeout})";
        in {
          # This trick prevents random `esc`s in situations where that's undesirable.
          capslock = "timeout(overload(control, esc), 500, layer(control))";
          # Might as well do it for the other thing.
          tab = "timeout(overload(meta, tab), 500, layer(meta))";
          a = lettermod "control" "a";
          s = lettermod "shift" "s";
          d = lettermod "alt" "d";
          f = lettermod "meta" "f";
          j = lettermod "meta" "j";
          k = lettermod "alt" "k";
          l = lettermod "shift" "l";
          ";" = lettermod "control" ";";
        };
      };
    };
  };
}

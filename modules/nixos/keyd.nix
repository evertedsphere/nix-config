{
  config,
  lib,
  pkgs,
  ...
}: {
  users.groups.keyd = {};

  systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [
    "CAP_SETGID"
    "CAP_SYS_NICE"
  ];

  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = ["*"];
      settings = {
        main = let
          overloadTimeout = 250;
          lettermod = layer: key: "overloadt(${layer}, ${key}, ${builtins.toString overloadTimeout})";
          abandonTimeout = 500;
          # This trick allows "cancelling" keypresses by holding them down for a while. This is particularly valuable for esc,
          # because an unwanted esc in various applications may abandon message composition or similar.
          modPrefMod = layer: key: "timeout(overload(${layer}, ${key}), ${builtins.toString abandonTimeout}, layer(${layer}))";
        in {
          capslock = modPrefMod "control" "esc";
          tab = modPrefMod "meta" "tab";
          a = lettermod "control" "a";
          s = lettermod "shift" "s";
          d = lettermod "alt" "d";
          f = lettermod "meta" "f";
          j = lettermod "meta" "j";
          k = lettermod "alt" "k";
          l = lettermod "shift" "l";
          ";" = lettermod "control" "enter";
          "enter" = ";";
        };
      };
    };
  };
}

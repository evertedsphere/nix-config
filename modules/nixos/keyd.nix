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
          # This trick allows "cancelling" keypresses by holding them down for a while. This is particularly valuable for esc,
          # because an unwanted esc in various applications may abandon message composition or similar.
          abandonTimeout = 500;
          modPrefMod = layer: key: "timeout(overload(${layer}, ${key}), ${builtins.toString abandonTimeout}, layer(${layer}))";

          # home row mods
          lettermod = timeout: layer: key: "overloadt(${layer}, ${key}, ${builtins.toString timeout})";
          lettermod2 = timeout: layer: key: "overloadt2(${layer}, ${key}, ${builtins.toString timeout})";
          shiftTimeout = 150;
          defaultTimeout = 250;
        in {
          "enter" = ";";

          capslock = modPrefMod "control" "esc";
          tab = modPrefMod "meta" "tab";

          "g" = lettermod defaultTimeout "ime" "g";
          "s" = lettermod defaultTimeout "meta" "s";
          "l" = lettermod defaultTimeout "meta" "l";
          ";" = lettermod defaultTimeout "control" "enter";
          "a" = lettermod defaultTimeout "control" "a";
          "f" = lettermod2 shiftTimeout "shift" "f";
          "j" = lettermod2 shiftTimeout "shift" "j";
          "k" = lettermod defaultTimeout "alt" "k";
          "d" = lettermod defaultTimeout "alt" "d";
        };

        "ime" = {
          "j" = "f13";
          "k" = "f14";
          "l" = "f15";
          # why not
          "h" = "esc";
        };
      };
    };
  };
}

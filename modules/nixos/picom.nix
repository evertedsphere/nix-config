{
  ...
}: {
  services.picom = let
    special = [
      "window_type *= 'menu'"
      "window_type *= 'utility'"
      "bounding_shaped && !rounded_corners"
    ];
  in {
    backend = "xrender";
    fade = true;
    vSync = true;
    fadeDelta = 6;
    fadeSteps = [0.028 0.032];
    fadeExclude = special;
    shadow = true;
    # shadowExclude = special ++ ["class_i = 'Dunst'"];
    shadowExclude = special;
    settings = {
      # https://github.com/dunst-project/dunst/issues/697#issuecomment-1188107553
      # prevents notifs from showing on lockscreen
      # but also basically disables the whole fucking compositor if you fullscreen mpv
      # unredir-if-possible = true;
      corner-radius = 10;
      shadow-offset-x = -20;
      shadow-offset-y = -20;
      shadow-radius = 20;
      shadow-opacity = 0.8;
      rounded-corners-exclude = special;
      blur = {
        method = "dual_kawase";
        strength = 8;
      };
      blur-background-exclude = special;
    };
  };
}

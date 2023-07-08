{
  config,
  lib,
  pkgs,
  ...
}: {
  services.keyd = {
    enable = true;
    ids = ["*"];
    settings = {
      main = {
        capslock = "overload(control, esc)";
        tab = "overload(meta, tab)";
        # "q" = "q";
        # "w" = "w";
        # "e" = "f";
        # "r" = "p";
        # "t" = "b";
        # "y" = "j";
        # "u" = "l";
        # "i" = "u";
        # "o" = "y";
        # "p" = ";";
        # "[" = "[";
        # "]" = "]";
        # "a" = "a";
        # "s" = "r";
        # "d" = "s";
        # "f" = "t";
        # "g" = "g";
        # "h" = "m";
        # "j" = "n";
        # "k" = "e";
        # "l" = "i";
        # ";" = "o";
        # "'" = "'";
        # "\\" = "\\";
        # "z" = "x";
        # "x" = "c";
        # "c" = "d";
        # "v" = "v";
        # "b" = "z";
        # "n" = "k";
        # "m" = "h";
        # "," = ",";
        # "." = ".";
        # "/" = "/";
        # "f12" = "toggle(enable_lefthand)";
      };
    #   enable_lefthand = {
    #     "space" = "overload(lefthand, space)";
    #     "leftshift" = "overload(shift, backspace)";
    #     "leftcontrol" = "enter";
    #   };
    #   lefthand = {
    #     # qwfpb jluy;
    #     # arstg mneio'\
    #     # xcdvz kh,./
    #     "q" = ";";
    #     "w" = "y";
    #     "e" = "u";
    #     "r" = "l";
    #     "t" = "j";
    #     "a" = "o";
    #     "s" = "i";
    #     "d" = "e";
    #     "f" = "n";
    #     "g" = "m";
    #     "h" = "'";
    #     "z" = "/";
    #     "x" = ".";
    #     "c" = ",";
    #     "v" = "h";
    #     "b" = "k";
    #   };
      };
  };
}

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
        "q" = "q";
        "w" = "w";
        "e" = "f";
        "r" = "p";
        "t" = "b";
        "y" = "j";
        "u" = "l";
        "i" = "u";
        "o" = "y";
        "p" = ";";
        "[" = "[";
        "]" = "]";
        "a" = "a";
        "s" = "r";
        "d" = "s";
        "f" = "t";
        "g" = "g";
        "h" = "m";
        "j" = "n";
        "k" = "e";
        "l" = "i";
        ";" = "o";
        "'" = "'";
        "\\" = "\\";
        "102nd" = "z";
        "z" = "x";
        "x" = "c";
        "c" = "d";
        "v" = "v";
        "b" = "z";
        "n" = "k";
        "m" = "h";
        "," = ",";
        "." = ".";
        "/" = "/";
      };
    };
  };
}

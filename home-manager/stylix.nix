{
  config,
  pkgs,
  ...
}: {
  stylix = {
    enable = true;
    # image = config.home.file.wallpaper.source;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
    autoEnable = false;
  };
  stylix.fonts = {
    serif = {
      package = pkgs.dejavu_fonts;
      name = "IBM Plex Serif";
    };

    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "IBM Plex Sans";
    };

    monospace = {
      package = pkgs.sarasa-gothic;
      name = "Sarasa Mono J";
    };

    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Emoji";
    };
  };
}

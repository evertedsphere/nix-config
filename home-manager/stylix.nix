{
  config,
  pkgs,
  ...
}: {
  stylix = {
    enable = true;
    image = config.home.file.wallpaper.source;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
    autoEnable = false;
  };
}

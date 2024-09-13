{pkgs, ...}: {
  stylix = {
    enable = true;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
    autoEnable = false;
    imageScalingMode = "fill";
    opacity = {
      terminal = 0.7;
    };
    targets.alacritty.enable = true;
    targets.helix.enable = true;
  };
}

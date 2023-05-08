{ config, lib, pkgs, ... }:

{
  fonts = {
    fontconfig = {
      defaultFonts = {
        monospace = [ "Iosevka Nerd Font" ];
        sansSerif = [ "DejaVu Sans" "IPAPGothic" ];
        serif = [ "DejaVu Serif" "IPAPMincho" ];
      };
    };
    fonts = with pkgs; [
      hanazono
      noto-fonts
      noto-fonts-extra
      noto-fonts-cjk
      noto-fonts-emoji
      carlito
      dejavu_fonts
      source-code-pro
      ttf_bitstream_vera
      iosevka
      google-fonts
      terminus_font
      (nerdfonts.override { fonts = [ "FiraCode" "Iosevka" "Monoid" ]; })
    ];
  };
}


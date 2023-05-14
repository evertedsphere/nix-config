{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    enableDefaultFonts = false;
    fonts = with pkgs; [
      material-symbols
      jost
      lexend
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
      (nerdfonts.override {fonts = ["FiraCode" "Iosevka" "Monoid"];})
    ];
    # user defined fonts
    # the reason there's Noto Color Emoji everywhere is to override DejaVu's
    # B&W emojis that would sometimes show instead of some Color emojis
    fontconfig = {
      defaultFonts = {
        serif = ["Noto Serif" "Noto Color Emoji"];
        sansSerif = ["Noto Sans" "Noto Color Emoji"];
        monospace = ["Iosevka" "BQN386 Unicode" "Noto Color Emoji"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}

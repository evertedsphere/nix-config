{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    enableDefaultPackages = false;
    packages = with pkgs; [
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
      source-han-sans
      source-han-mono
      source-code-pro
      iosevka-comfy.comfy
      iosevka-comfy.comfy-duo
      noto-fonts
      noto-fonts-extra
      noto-fonts-cjk
      noto-fonts-emoji
      google-fonts
      terminus_font
    ];
    # user defined fonts
    # the reason there's Noto Color Emoji everywhere is to override DejaVu's
    # B&W emojis that would sometimes show instead of some Color emojis
    fontconfig = {
      # TODO parity with home-manager.local.fonts.*
      # TODO ditto for stylix.fonts.*
      defaultFonts = {
        serif = ["IBM Plex Serif" "Noto Color Emoji"];
        sansSerif = ["IBM Plex Sans" "Noto Color Emoji"];
        monospace = ["Iosevka Comfy" "BQN386 Unicode" "Noto Color Emoji"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}

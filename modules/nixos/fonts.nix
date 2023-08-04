{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    enableDefaultPackages = false;
    packages = with pkgs; [
      sarasa-gothic-nerd-fonts
      source-han-sans
      source-han-mono
      source-code-pro
      hanazono
      noto-fonts
      noto-fonts-extra
      noto-fonts-cjk
      noto-fonts-emoji
      # material-symbols
      # jost
      # lexend
      # carlito
      # dejavu_fonts
      # ttf_bitstream_vera
      # iosevka
      google-fonts
      terminus_font
    ];
    # user defined fonts
    # the reason there's Noto Color Emoji everywhere is to override DejaVu's
    # B&W emojis that would sometimes show instead of some Color emojis
    fontconfig = {
      # TODO parity with home-manager.local.fonts.*
      defaultFonts = {
        serif = ["Noto Serif" "Noto Color Emoji"];
        sansSerif = ["Noto Sans" "Noto Color Emoji"];
        monospace = ["Sarasa Mono J Nerd Font" "BQN386 Unicode" "Noto Color Emoji"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };
}

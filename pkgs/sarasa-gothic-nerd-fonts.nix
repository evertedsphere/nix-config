{
  pkgs,
  lib,
  fetchurl,
}: let
  pname = "sarasa-gothic-nerd-fonts";
  version = "0.40.7-0";
in
  fetchurl {
    name = "${pname}-${version}";

    url = "https://github.com/jonz94/Sarasa-Gothic-Nerd-Fonts/releases/download/v${version}/sarasa-mono-j-nerd-font.zip";
    sha256 = "sha256-ce9oezo82StZT8g18CceceVNpFhnxT118JdrojpTtsk=";

    recursiveHash = true;
    downloadToTemp = true;
    postFetch = ''
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
    '';

    meta = with lib; {
      description = "Sarasa Gothic Nerd Fonts";
      longDescription = "Sarasa patched to include Nerd Font glyphs";
      homepage = "https://github.com/jonz94/Sarasa-Gothic-Nerd-Fonts";
      license = licenses.ofl;
      platforms = platforms.all;
      maintainers = with maintainers; [];
    };
  }

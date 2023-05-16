{ pkgs, lib, fetchzip }:

let
  pname = "sarasa-gothic-nerd-fonts";
  version = "0.40.7-0";
in fetchzip {
  name = "${pname}-${version}";

  url = "https://github.com/jonz94/Sarasa-Gothic-Nerd-Fonts/releases/download/v${version}/sarasa-mono-j-nerd-font.zip";

  sha256 = "sha256-8p15thg3xyvCA/8dH2jGQoc54nzESFDyv5m47FgWsSI=";

  postFetch = ''
    ${pkgs.unzip}/bin/unzip $downloadedFile
    install -m444 -Dt $out/share/fonts/truetype */*.ttf
  '';

  meta = with lib; {
    description = "Sarasa Gothic Nerd Fonts";
    longDescription = "Sarasa patched to include Nerd Font glyphs";
    homepage = "https://github.com/jonz94/Sarasa-Gothic-Nerd-Fonts";
    license = licenses.ofl;
    platforms = platforms.all;
    maintainers = with maintainers; [ ];
  };
}
  # postFetch = ''
  #   mkdir -p $out/share/fonts/opentype
  #   unzip -j $downloadedFile \*.otf -d $out/share/fonts/opentype
  # '';

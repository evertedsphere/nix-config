{
  stdenv,
  lib,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "BQN386";
  version = "20211026";
  src = fetchFromGitHub {
    owner = "dzaima";
    repo = "BQN386";
    rev = "af9438073ec0797d8e7ebaf67e6c6b744e82fa18";
    sha256 = "sha256-Fv0im+ehnTkHV95f1FuFQDQ3bif+Du9JPZB7xSPiSpE=";
  };
  phases = ["unpackPhase" "installPhase"];

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    install -m 644 BQN386.ttf $out/share/fonts/truetype
  '';

  meta = with lib; {
    description = "Font for APL and BQN";
    platforms = platforms.all;
  };
}

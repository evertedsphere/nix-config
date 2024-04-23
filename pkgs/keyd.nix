{
  stdenv,
  lib,
  fetchFromGitHub,
  systemd,
  runtimeShell,
  python3,
  nixosTests,
}: let
  version = "git";

  src = fetchFromGitHub {
    owner = "rvaiya";
    repo = "keyd";
    rev = "85dc69b57c38d432147a73940fc28252450fdcac";
    hash = "sha256-XLWbun+jazkS0L8ywv5wlBFcw1WKk55UOfQaq8j71jw=";
  };

  pypkgs = python3.pkgs;

  appMap = pypkgs.buildPythonApplication rec {
    pname = "keyd-application-mapper";
    inherit version src;
    format = "other";

    postPatch = ''
      substituteInPlace scripts/${pname} \
        --replace /bin/sh ${runtimeShell}
    '';

    propagatedBuildInputs = with pypkgs; [xlib];

    dontBuild = true;

    installPhase = ''
      install -Dm555 -t $out/bin scripts/${pname}
    '';

    meta.mainProgram = "keyd-application-mapper";
  };
in
  stdenv.mkDerivation {
    pname = "keyd";

    inherit version src;
    installFlags = ["DESTDIR=${placeholder "out"}" "PREFIX="];

    postInstall = ''
      ln -sf ${lib.getExe appMap} $out/bin/${appMap.pname}
    '';

    meta = with lib; {
      description = "A key remapping daemon for Linux";
      license = licenses.mit;
      maintainers = with maintainers; [];
      platforms = platforms.linux;
    };
  }

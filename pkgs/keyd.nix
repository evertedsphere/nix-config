{
  stdenv,
  lib,
  fetchFromGitHub,
  systemd,
  runtimeShell,
  python3,
  nixosTests,
}:
stdenv.mkDerivation {
  pname = "keyd";
  version = "git";

  src = fetchFromGitHub {
    owner = "rvaiya";
    repo = "keyd";
    rev = "85dc69b57c38d432147a73940fc28252450fdcac";
    hash = "sha256-XLWbun+jazkS0L8ywv5wlBFcw1WKk55UOfQaq8j71jw=";
  };

  installFlags = ["DESTDIR=${placeholder "out"}" "PREFIX="];

  meta = with lib; {
    description = "A key remapping daemon for Linux";
    license = licenses.mit;
    maintainers = with maintainers; [];
    platforms = platforms.linux;
  };
}

{ config, lib, pkgs, ... }:

let
  org-protocol-mime = "x-scheme-handler/org-protocol";
  name = "org-protocol";
in
{
  xdg.mimeApps.defaultApplications."${org-protocol-mime}" = "${name}.desktop";
  home.packages = [
    (pkgs.makeDesktopItem {
      inherit name;
      exec = "emacsclient %u";
      comment = "${name} handler";
      desktopName = "${name}";
      type = "Application";
      mimeTypes = [org-protocol-mime];
    })
  ];
}

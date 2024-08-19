{
  config,
  lib,
  pkgs,
  ...
}: let
  org-protocol-mime = "x-scheme-handler/org-protocol";
  name = "org-protocol";
in {
  # Without this, the default handler actually ends up being emacsclient,
  # which causes it to pop up a new frame and completely ignore the request.
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

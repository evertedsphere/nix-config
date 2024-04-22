{pkgs ? (import ../nixpkgs.nix) {}}: {
  keyd-git = pkgs.callPackage ./keyd.nix {  };
}

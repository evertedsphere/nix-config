{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts.packages = [(pkgs.callPackage ./font.nix {})];

  environment.systemPackages = with pkgs; [
    cbqn
  ];

  # remember to use us,bqn and set a switch keybind
  services.xserver.extraLayouts.bqn = {
    description = "bqn symbols, available in xkeyboard-config >= 2.36";
    languages = ["eng"];
    symbolsFile = ./xkb_symbols;
  };

  fonts.fontconfig.localConf = ''
    <alias>
      <family>APL385 Unicode</family>
      <prefer><family>BQN386 Unicode</family></prefer>
      <default><family>monospace</family></default>
    </alias>
  '';
}

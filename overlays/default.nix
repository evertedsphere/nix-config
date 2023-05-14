# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    waybar = prev.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    });
    # xorg = prev.xorg //
    #   { xkeyboard_config = prev.xkeyboard_config.overrideAttrs(old: rec {
    #   name = "xkeyboard-config-2.36";
    #   url = "mirror://xorg/individual/data/xkeyboard-config/xkeyboard-config-2.36.tar.bz2";
    #   sha256 = "2g6kn7l0mixw50kgn7897gwv1990c5rczr2x776q3xywss8dfzv5";
    #     }); } ;
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}

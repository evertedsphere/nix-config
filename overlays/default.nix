# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: prev:
    import ../pkgs {
      pkgs = final;
    };

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    keyd = prev.keyd-git;
    python312 = prev.python312.override {packageOverrides = _: pysuper: {nose = pysuper.pynose;};};
    formats = prev.formats // {
      ron = import ./ron.nix {inherit (prev) lib pkgs;};
    };
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    # unstable = import inputs.nixpkgs-unstable {
    #   system = final.system;
    #   config.allowUnfree = true;
    # };
  };
}

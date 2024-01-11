{
  description = "Your new nix config";

  inputs = {
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-colors.url = "github:misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    impermanence.url = "github:nix-community/impermanence";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    emacs-overlay,
    ...
  } @ inputs: let
    inherit (self) outputs;
    forAllSystems = nixpkgs.lib.genAttrs [
      "x86_64-linux"
      # "aarch64-linux" "i686-linux" "aarch64-darwin" "x86_64-darwin"
    ];
  in {
    # Your custom packages
    # Acessible through 'nix build', 'nix shell', etc
    packages = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        import ./pkgs {inherit pkgs;}
    );
    # Devshell for bootstrapping
    # Acessible through 'nix develop' or 'nix-shell' (legacy)
    devShells = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        import ./shell.nix {inherit pkgs;}
    );

    # Your custom packages and modifications, exported as overlays
    overlays = import ./overlays {inherit inputs;};

    # Reusable nixos modules you might want to export
    # These are usually stuff you would upstream into nixpkgs
    nixosModules = import ./modules/nixos;

    # Reusable home-manager modules you might want to export
    # These are usually stuff you would upstream into home-manager
    homeManagerModules = import ./modules/home-manager;

    # nixos-rebuild --flake .#your-hostname
    nixosConfigurations = let
      mkSystem = hostname: let
        nhConfig.nh = {
          enable = true;
          clean.enable = true;
          clean.extraArgs = "--keep-since 10d --keep 10";
        };
        homeManagerConfig.home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.s = import ./hosts/${hostname}/home-manager/home.nix;
          extraSpecialArgs = {inherit inputs outputs;};
        };
      in
        nixpkgs.lib.nixosSystem {
          specialArgs = {inherit inputs outputs;};
          modules = [
            ./nixos/configuration.nix
            ./hosts/${hostname}/configuration.nix
            home-manager.nixosModules.home-manager
            homeManagerConfig
            inputs.nh.nixosModules.default
            nhConfig
          ];
        };
    in {
      malina = mkSystem "malina";
      work = mkSystem "work";
    };

    # home-manager --flake .#your-username@your-hostname
    # I do it this way because rebuilds are much faster.
    # homeConfigurations = {
    #   "s@malina" = home-manager.lib.homeManagerConfiguration {
    #     pkgs = nixpkgs.legacyPackages.x86_64-linux;
    #     extraSpecialArgs = {inherit inputs outputs;};
    #     modules = [
    #       ./home-manager/config.nix
    #       ./home-manager/home.nix
    #       ./hosts/malina/home-manager/home.nix
    #     ];
    #   };
    # };
  };
}

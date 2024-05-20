{
  description = "bech32";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      # disabling to reduce CI time initially. Uncomment later
      #"x86_64-darwin"
      #"aarch64-linux"
      #"aarch64-darwin"
    ];
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [
            inputs.iohkNix.overlays.crypto
            inputs.haskellNix.overlay
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc965";
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "bech32";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            {
              packages.bech32.configureFlags = ["--ghc-option=-Werror"];
            }
          ];
        });
        # ... and construct a flake from the cabal project
        flake = cabalProject.flake (
          lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs ["ghc8107"] (compiler-nix-name: {
              inherit compiler-nix-name;
            });
          }
        );
      in
        lib.recursiveUpdate flake rec {
          project = cabalProject;
          # add a required job, that's basically all hydraJobs.
          hydraJobs = flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                };
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}

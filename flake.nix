{
  description = "Haskell MCMC development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  inputs.mcmc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";
  inputs.dschrempf-nur.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      mcmc,
      dschrempf-nur,
    }:
    let
      theseHpkgNames = [
        "munihac24-mcmc"
      ];
      thisGhcVersion = "ghc96";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides =
            selfh: superh:
            supern.haskell.packageOverrides selfh superh
            // {
              munihac24-mcmc = selfh.callCabal2nix "munihac24-mcmc" ./. { };
            };
        };
      };
      overlays = [
        hOverlay
        mcmc.overlays.default
      ];
      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
          dschrempf = dschrempf-nur.packages.${system};
        in
        {
          packages = theseHpkgs // {
            default = theseHpkgs.munihac24-mcmc;
          };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = [ dschrempf.tracer ];
            # doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;
    }
    // flake-utils.lib.eachDefaultSystem perSystem;
}

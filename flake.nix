{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        # `nix build`
        packages.bad-datalog = pkgs.stdenv.mkDerivation {
          name = "bad-datalog";
          src = ./.;

          buildInputs = [ pkgs.elmPackages.elm ];

          buildPhase = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./nix/elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./nix/registry.dat;
          };

          # it's a little weird that elm2nix says it's fine to build in the install
          # phase, but who am I to judge?
          installPhase = ''
            mkdir -p $out/share/bad-datalog

            (
              cd sample-apps
              elm make src/Main.elm --output $out/share/bad-datalog/index.html
            )
          '';
        };
        defaultPackage = packages.bad-datalog;
        overlay = final: prev: { bad-datalog = packages.bad-datalog; };

        # `nix develop`
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git

            elm2nix
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-json
            elmPackages.elm-live
            elmPackages.elm-review
            elmPackages.elm-test
          ];
        };
      });
}

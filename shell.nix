{ ... }:
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "bad-datalog";
  buildInputs = [
    niv.niv
    git
    gnumake

    # elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    elm2nix
  ];
}

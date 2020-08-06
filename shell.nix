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

    # elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
  ];
}

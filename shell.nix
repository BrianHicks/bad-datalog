{ ... }:
let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in pkgs.mkShell {
  buildInputs = [
    niv.niv
    pkgs.git

    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-json
  ];
}

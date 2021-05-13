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
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-json
    pkgs.elmPackages.elm-live
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-test
  ];
}

{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "datalog";
  src = gitignore.gitignoreSource ./.;

  buildInputs = [ elmPackages.elm elmPackages.elm-test ];
  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./nix/elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./nix/registry.dat;
  };

  doCheck = true;
  checkPhase = ''
    elm-test
  '';

  installPhase = ''
    mkdir -p $out/share/datalog
    cd playground
    elm make --optimize --output $out/share/datalog/index.html src/Main.elm
  '';
}

{ ... }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in rec {
  datalog = pkgs.stdenv.mkDerivation {
    name = "datalog";
    src = gitignore.gitignoreSource ./.;

    buildInputs = [ pkgs.elmPackages.elm pkgs.elmPackages.elm-test ];
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
  };

  container = let
    linuxPkgs = import sources.nixpkgs { system = "x86_64-linux"; };
    listenPort = "80";
  in linuxPkgs.dockerTools.buildLayeredImage {
    name = "datalog";
    contents = linuxPkgs.darkhttpd;
    config = {
      Cmd = [ "darkhttpd" "${datalog}/share/datalog" ];
      ExposedPorts = { "${listenPort}/tcp" = { }; };
    };
  };
}

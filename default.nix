{ sources ? import ./nix/sources.nix { }, nixpkgs ? import sources.nixpkgs { }
}:
let gitignore = nixpkgs.callPackage sources.gitignore { };
in nixpkgs.stdenv.mkDerivation {
  name = "bad-datalog";

  src = gitignore.gitignoreSource ./.;
  buildInputs = [ nixpkgs.elmPackages.elm nixpkgs.elmPackages.elm-test ];

  buildPhase = nixpkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./nix/elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./nix/registry.dat;
  };

  doCheck = true;
  checkPhase = "elm-test";

  # it's a little weird that elm2nix says it's fine to build in the install
  # phase, but who am I to judge?
  installPhase = ''
    mkdir -p $out/share/bad-datalog

    (
      cd sample-apps
      elm make src/Main.elm --output $out/share/bad-datalog/index.html
    )
  '';
}

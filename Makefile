nix/elm-srcs.nix: playground/elm.json
	cd playground && elm2nix convert > ../$@

nix/registry.dat: playground/elm.json
	cd playground && elm2nix snapshot
	mv playground/registry.dat $@

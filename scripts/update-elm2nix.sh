#!/usr/bin/env bash
set -euo pipefail
set -x

(
  cd sample-app

  elm2nix convert > ../nix/elm-srcs.nix
  elm2nix snapshot
  mv registry.dat ../nix/registry.dat
)

#!/usr/bin/env bash
set -euo pipefail

cd sample-app
elm-live --pushstate index.html src/Main.elm

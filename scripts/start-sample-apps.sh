#!/usr/bin/env bash
set -euo pipefail

cd sample-apps
elm-live --pushstate index.html src/Main.elm

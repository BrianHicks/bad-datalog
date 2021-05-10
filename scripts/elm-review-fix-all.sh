#!/usr/bin/env bash
set -euo pipefail

elm-review --elm-format-path "$(which elm-format)" --fix-all

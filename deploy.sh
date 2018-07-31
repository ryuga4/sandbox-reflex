#!/usr/bin/env bash

# Compiling with ghcjs:
stack build --stack-yaml=stack-ghcjs.yaml

# Moving the generated files to the js folder:
mkdir -p js
cp -r $(stack path --local-install-root --stack-yaml=stack-ghcjs.yaml)/bin/sandbox-reflex.jsexe/all.js js/

# Minifying all.js file using the closure compiler:
#cd js
#google-closure-compiler-js all.js --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js

# OPTIONAL: zipping, to see the actual transferred size of the app:
#sbzopfli all.min.js

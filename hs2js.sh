# Compile Haskell module $1 to $1.js
#
# Argument $1: Haskell module to compile(should be included in cabal file)
#
# Better to run in a clean directory
#
# requires: yay -S ghcup rollup emscripten emsdk
#
# See https://www.haskell.org/ghcup/guide/#ghc-wasm-cross-bindists-experimental
#
# Installation(arch linux):
#
# yay -S ghcup rollup emscripten emsdk
# ghcup config add-release-channel cross
# echo 'PATH="$HOME/.ghcup/bin:$PATH"' >> ~/.bashrc
# source /usr/lib/emsdk/emsdk_env.sh
# emconfigure ghcup install ghc --set javascript-unknown-ghcjs-9.6.2
#
# Usage: hs2js.sh [module.hs] [output.js]

set -e # Quit on errors

SOURCE=${1:-Main.hs}
SOURCE_COMPILED=${SOURCE%.hs}

if [[ -n "$2" ]]; then
  TARGET="$2"
else
  TARGET="${SOURCE%.hs}.js"
fi

echo Compiling dependencies...
cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg --only-dependencies

echo "$SOURCE -> $SOURCE_COMPILED..."
cabal --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg exec bash -- -c "javascript-unknown-ghcjs-ghc -fexternal-interpreter -Wall $SOURCE"

rollup $SOURCE_COMPILED -o $TARGET

echo Cleaning up...
rm -rf *.jsexe *.o *.hi $SOURCE_COMPILED

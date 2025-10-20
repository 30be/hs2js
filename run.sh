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

set -- "${1:-Main}" "${@:2}" # Default is Main

cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg --only-dependencies

set -e
cabal --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg exec bash -- -c "javascript-unknown-ghcjs-ghc $@"

rollup $1 -o $1.js
# mv $1 $1.js

rm -rf *.jsexe *.o *.hi

if [[ -f "$1" && "$1" != *".hs" ]]; then
  rm "$1"
fi

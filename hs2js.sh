# Compile Haskell module $1 to $1.js
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

cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg
echo "Build errors just above are OK"

cabal --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg exec bash -- -c "javascript-unknown-ghcjs-ghc $@"

rollup $1 -o $1.js
rm -rf $1.jsexe $1.o $1.hi

if [[ "$1" != *".hs" ]]; then
  rm $1
fi

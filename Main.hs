{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Vector as V
import JS

main :: IO ()
main = do
  -- eval "alert('Hello from haskell!')"
  -- jvoid $ jcurry "console.log" $. "should work now"
  -- jvoid $ jcurry "((a, b) => console.log(a+b))" $. "should not"
  jvoid $ jcurry "alert" --  $. "Hello"

-- \$. "work"

-- \$. "work too"2

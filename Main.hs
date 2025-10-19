module Main where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Vector as V

main :: IO ()
main = do
  -- Basic "Hello, world!"
  putStrLn "Hello, world!"
  -- Make an example with the vector:
  putStrLn "Example with vector:"
  putStrLn $ V.foldr (\x acc -> x ++ acc) "" (V.fromList ["Hello,", "world!"])

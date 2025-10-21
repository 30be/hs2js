-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Vector as V
import GHC.JS.Prim (JSVal, toJSInt, toJSString)
import JS
import Relude

log :: (ToJSVal a) => a -> IO ()
log a = void $ (console .>> "log") >>= llac a

-- void $ call <$> (console .>> "log") ?? a

plot :: String -> IO ()
plot id = do
  log "Hello log from JS"
  (console .>> "log") >>= llac id
  canvas <- (document .>> "getElementById") >>= llac id
  ctx <- join $ call <$> canvas .> "getContext" ?? "2d"
  computedStyle <- join (call <$> window .>> "getComputedStyle" <*> (document .>> "body")) .>> "color"
  log computedStyle

  let padding = 70 :: Int
      width = 700 :: Int
      height = 300 :: Int
  set canvas "width" width
  set canvas "height" height

  ctx .> "beginPath" >>= call0
  jvoid $ jcurry (ctx .> "arc") $. (150 :: Int) $. (150 :: Int) $. (80 :: Int) $. (0 :: Int) $. (2 * 3.14159 :: Double)
  set ctx "fillStyle" "#10b981"
  ctx .> "fill" >>= call0
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" (4 :: Int)
  ctx .> "stroke" >>= call0
  void $ ctx .> "closePath" >>= call0

main :: IO ()
main = plot "canvas"

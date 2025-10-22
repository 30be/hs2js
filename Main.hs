{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Vector as V
import GHC.JS.Prim (JSVal, toJSInt, toJSString)
import JS
import Relude

log :: (ToJSVal arg) => arg -> IO ()
log a = void $ window .>> "console" .>> "log" >>= call @(JSVal -> JSVal -> [JSVal]) (toJSVal a)

arc :: JSVal -> Int -> Int -> Int -> Double -> Double -> IO JSVal
arc ctx cx cy r start end = ctx .> "arc" >>= call @(Int -> Int -> Int -> Double -> Double -> JSVal -> [JSVal]) cx cy r start end

plot :: String -> IO ()
plot id = do
  log "Hello log from JS"
  window .>> "alert" >>= call @(String -> JSVal -> [JSVal]) "Hello alert from JS"
  canvas <- window .>> "document" .>> "getElementById" >>= call @(String -> JSVal -> [JSVal]) id

  let width = 700 :: Int
      height = 300 :: Int
  set canvas "width" width
  set canvas "height" height

  ctx <- canvas .> "getContext" >>= call @(String -> JSVal -> [JSVal]) "2d"
  ctx ~> "beginPath"
  arc ctx 150 150 80 0 (2 * 3.14159)
  set ctx "fillStyle" "#10b981"
  ctx ~> "fill"
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" (4 :: Int)
  ctx ~> "stroke"
  ctx ~> "closePath"
  window .>> "alert" >>= call @(String -> JSVal -> [JSVal]) "polyvariadic"
  void $ window .>> "console" .>> "log" >>= call @(String -> String -> String -> JSVal -> [JSVal]) "hello" "world" "polyvariadic"

main :: IO ()
main = plot "canvas"

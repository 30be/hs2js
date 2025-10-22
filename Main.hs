{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import GHC.JS.Prim (JSVal)
import JS
import Relude

log :: (ToJSVal arg) => arg -> IO ()
log a = void $ window .>> "console" .>> "log" >>= \f -> (call f (toJSVal a) :: IO JSVal)

arc :: JSVal -> Int -> Int -> Int -> Double -> Double -> IO JSVal
arc ctx cx cy r start end = ctx .> "arc" >>= \f -> call f cx cy r start end

plot :: String -> IO ()
plot canvasId = do
  log "Hello log from JS"
  window .>> "alert" >>= \f -> (call f "Hello alert from JS" :: IO JSVal)
  canvas <- window .>> "document" .>> "getElementById" >>= \f -> (call f canvasId :: IO JSVal)

  let width = 700 :: Int
      height = 300 :: Int
  set canvas "width" width
  set canvas "height" height

  ctx <- canvas .> "getContext" >>= \f -> (call f "2d" :: IO JSVal)
  ctx ~> "beginPath"
  arc ctx 150 150 80 0 (2 * 3.14159)
  set ctx "fillStyle" "#10b981"
  ctx ~> "fill"
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" (4 :: Int)
  ctx ~> "stroke"
  ctx ~> "closePath"
  window .>> "alert" >>= \f -> (call f "polyvariadic" :: IO JSVal)
  void $ window .>> "console" .>> "log" >>= \f -> (call f "hello" "world" "polyvariadic" :: IO JSVal)

main :: IO ()
main = plot "canvas"

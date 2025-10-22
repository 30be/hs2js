-- Sorry but we work with JS
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import JS
import Relude

log :: (ToJSVal arg) => arg -> IO ()
log a = window ~> "console" ~> "log" >>= call ?? toJSVal a

plot :: String -> IO ()
plot canvasId = do
  log "Hello log from JS"
  canvas <- window ~> "document" ~> "getElementById" >>= call ?? canvasId
  set canvas "width" 700
  set canvas "height" 300

  ctx <- canvas .> "getContext" >>= call ?? "2d"
  ctx .> "beginPath" >>= call :: IO ()
  ctx .> "arc" >>= \f -> call f 150 150 80 0 (2 * 3.14159) :: IO ()
  set ctx "fillStyle" "#10b981"
  ctx .> "fill" >>= call :: IO ()
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" 4
  ctx .> "stroke" >>= call :: IO ()
  ctx .> "closePath" >>= call :: IO ()
  window ~> "alert" >>= call ?? "Hello alert from JS" :: IO ()
  window ~> "alert" >>= call ?? "polyvariadic" :: IO ()
  window ~> "console" ~> "log" >>= call ?? "hello" ?? "world" ?? "polyvariadic" :: IO ()

main :: IO ()
main = plot "canvas"

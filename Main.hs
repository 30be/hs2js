-- Sorry but we work with JS
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import JS
import Relude

plot :: String -> IO ()
plot canvasId = do
  canvas <- window ~> "document" ~> "getElementById" >>= call ?? canvasId
  set canvas "width" 700
  set canvas "height" 300

  ctx <- canvas .> "getContext" >>= call ?? "2d"
  set ctx "fillStyle" "#10b981"
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" 4
  ctx .> "beginPath" >>= call :: IO ()
  ctx .> "arc" >>= \f -> call f 150 150 80 0 (2 * 3.14159) :: IO ()
  ctx .> "fill" >>= call :: IO ()
  ctx .> "stroke" >>= call :: IO ()
  ctx .> "closePath" >>= call :: IO ()

main :: IO ()
main = do
  putStrLn "Hello log from JS"
  plot "canvas"
  window ~> "alert" >>= call ?? "Hello alert from JS" :: IO ()
  window ~> "console" ~> "log" >>= call ?? "hello" ?? "world" ?? "polyvariadic" :: IO ()

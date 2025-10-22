{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Vector as V
import GHC.JS.Prim (JSVal, toJSInt, toJSString)
import JS
import Relude

log :: (ToJSVal a) => a -> IO ()
log a = void $ (window .>> "console" .>> "log") >>= llac a

arc :: JSVal -> Int -> Int -> Int -> Int -> Double -> IO JSVal
arc ctx cx cy r start end = (ctx .> "arc") >>= \f -> call5 f cx cy r start end

plot :: String -> IO ()
plot id = do
  log "Hello log from JS"
  window .>> "alert" >>= llac "Hello alert from JS"
  window .>> "console" .>> "log" >>= llac id
  canvas <- window .>> "document" .>> "getElementById" >>= llac id
  ctx <- canvas .> "getContext" >>= llac "2d"

  let width = 700 :: Int
      height = 300 :: Int
  set canvas "width" width
  set canvas "height" height

  ctx ~> "beginPath"
  arc ctx 150 150 80 0 (2 * 3.14159)
  set ctx "fillStyle" "#10b981"
  ctx ~> "fill"
  set ctx "strokeStyle" "#065f46"
  set ctx "lineWidth" (4 :: Int)
  ctx ~> "stroke"
  -- (~>) @(JSVal -> String -> JSVal -> IO JSVal) ctx "closePath"
  -- ctx ~> "closePath"
  -- void $ (ctx ~> "closePath")
  void $ (~>) @([JSVal]) ctx "closePath"
  alert <- window .>> "alert"
  void $ call @(JSVal -> JSVal -> [JSVal]) alert (toJSString "polyvariadic")

main :: IO ()
main = plot "canvas"

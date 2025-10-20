-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Vector as V
import GHC.JS.Prim (JSVal, toJSString)
import JS
import Relude

log :: (ToJSVal a) => a -> IO ()
log a = void $ (console .$>> "log") >>= llac a

-- void $ call <$> (console .>> "log") ?? a

plot :: String -> IO ()
plot id = do
  log "Hello log from JS"
  (console .$>> "log") >>= llac id
  canvas <- (document .$>> "getElementById") >>= llac id
  ctx <- join $ call <$> canvas .> "getContext" ?? "2d"
  computedStyle <- join (call <$> window .>> "getComputedStyle" <*> (document .>> "body")) .>> "color"
  let padding = 70 :: Int
      width = 700 :: Int
      height = 300 :: Int
  set canvas "width" width
  set canvas "height" height
  set ctx "strokeStyle" computedStyle
  set ctx "fillStyle" computedStyle
  set ctx "lineWidth" (1.5 :: Double)
  set ctx "font" "20px sans-serif"
  set ctx "textAlign" "center"

  jvoid $ jcurry (ctx .$> "clearRect") $. (0 :: Int) $. (0 :: Int) $. width $. height
  jvoid $ ctx .> "beginPath" >>= call0
  jvoid $ jcurry (ctx .$> "moveTo") $. padding $. padding
  jvoid $ ctx .> "lineTo" >>= call2 ?? padding ?? (height - padding :: Int)

main :: IO ()
main = do
  plot "canvas"

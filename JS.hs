{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Micro library for working with JS
-- foreign import javascript "((fn) =>{\
--    \  const curried = (...args) => args.length >= fn.length ? fn(...args) : (...nextArgs) => curried(...args, ...nextArgs); \
--    \  curried.fname = fn.name;\
--    \  return curried;\
--    \})"
--   jcurry_ :: JSVal -> IO JSVal
--
-- foreign import javascript "(fn => { if (typeof fn === 'function') {console.error(`Not enough arguments for function ${fn.fname} (${fn.name})`);} })" noignore :: JSVal -> IO JSVal : w
-- jcurry :: (ToJSVal a) => IO a -> IO JSVal -- todo: merge that with jvoid with rewriting in the style of printf
-- jcurry val = val >>= toJSVal >>= jcurry_
-- jvoid :: IO JSVal -> IO () -- Works like void but says if you have tried to execute a function without enough elements and not just silently ignores the result
-- jvoid = (>>= noignore >>> void)
-- No idea how to do that better now
-- https://okmij.org/ftp/Haskell/polyvar-comp.lhs
-- call :: JSVal -> JSVal -> IO JSVal -- temporary

module JS where

import Control.Arrow ((>>>))
import Control.Monad (replicateM_, void, when, (>=>))
import GHC.JS.Prim (JSVal, fromJSInt, toJSArray, toJSInt, toJSString)
import Relude
import System.IO.Unsafe (unsafePerformIO)

foreign import javascript "(()=>window)" window :: IO JSVal

foreign import javascript "((el, sel) => ((typeof el[sel] === 'function') ? el[sel].bind(el) : el[sel]))" attr :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel, val) => {el[sel] = val })" set_ :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "((obj) => {new obj()})" new :: JSVal -> IO JSVal

foreign import javascript "((f, list) => f(...list))" callList_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript "(f => f())" call0 :: JSVal -> IO JSVal

(.>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a .> b = attr a (toJSVal b)

(.>>) :: (ToJSVal b) => IO JSVal -> b -> IO JSVal
a .>> b = a >>= (.> b)

set :: (ToJSVal a) => (ToJSVal b) => JSVal -> b -> a -> IO ()
set obj key val = void $ set_ obj (toJSVal key) (toJSVal val)

class ToJSVal a where
  toJSVal :: a -> JSVal

instance ToJSVal Int where
  toJSVal = toJSString . show

instance ToJSVal Double where
  toJSVal = toJSString . show

instance ToJSVal String where
  toJSVal = toJSString

instance ToJSVal JSVal where
  toJSVal = id

instance ToJSVal [JSVal] where
  toJSVal = unsafePerformIO . toJSArray

class ListBuilder r where
  build :: [JSVal] -> r

instance ListBuilder [JSVal] where
  build = id

instance (ToJSVal a, ListBuilder next_r) => ListBuilder (a -> next_r) where
  build acc x = build ((toJSVal x) : acc)

class MCompose f gresult result | f gresult -> result where
  mcomp :: f -> ([JSVal] -> gresult) -> result

instance MCompose (a -> [JSVal]) c (a -> c) where
  mcomp f g = g . f

instance (MCompose (f -> f') gresult result) => MCompose (a -> f -> f') gresult (a -> result) where
  mcomp f g a = mcomp (f a) g

callList :: JSVal -> [JSVal] -> IO JSVal
callList f list = toJSArray list >>= callList_ f

-- Gets arguments first and the function last
-- Should be called with type argument(for now):
-- call @(Arg1 -> Arg2 -> ... -> ArgN -> JSVal -> [JSVal])
call :: forall builder result. (ListBuilder builder, MCompose builder (IO JSVal) result) => result
call =
  (build [] :: builder) `mcomp` \case
    [] -> error "call requires at least one argument"
    (x : xs) -> callList x xs

-- Call member function, like
-- window .>> "history" ~> "back"
(~>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a ~> b = a .> b >>= call0

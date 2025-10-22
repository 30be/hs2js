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

foreign import javascript "(()=>window)" window :: IO JSVal

foreign import javascript "((el, sel) => ((typeof el[sel] === 'function') ? el[sel].bind(el) : el[sel]))" attr :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel, val) => {el[sel] = val })" set_ :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "((obj) => {new obj()})" new :: JSVal -> IO JSVal

(.>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a .> b = toJSVal b >>= attr a

(.>>) :: (ToJSVal b) => IO JSVal -> b -> IO JSVal
a .>> b = a >>= (.> b)

llac :: (ToJSVal a) => (ToJSVal b) => a -> b -> IO JSVal
llac = flip call1

set :: (ToJSVal a) => (ToJSVal b) => JSVal -> b -> a -> IO ()
set obj key val = do
  k <- toJSVal key
  v <- toJSVal val
  void $ set_ obj k v

class ToJSVal a where
  toJSVal :: a -> IO JSVal

instance ToJSVal Int where
  toJSVal = pure . toJSString . show

instance ToJSVal Double where
  toJSVal = pure . toJSString . show

instance ToJSVal String where
  toJSVal = pure . toJSString

instance ToJSVal JSVal where
  toJSVal = pure

instance ToJSVal [JSVal] where
  toJSVal = toJSArray

foreign import javascript "(el => el())" call0 :: JSVal -> IO JSVal

foreign import javascript "((el, a1) => el(a1))" call1_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2) => el(a1, a2))" call2_ :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3) => el(a1, a2, a3))" call3_ :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4) => el(a1, a2, a3, a4))" call4_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4, a5) => el(a1, a2, a3, a4, a5))" call5_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4, a5, a6) => el(a1, a2, a3, a4, a5, a6))" call6_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4, a5, a6, a7) => el(a1, a2, a3, a4, a5, a6, a7))" call7_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4, a5, a6, a7, a8) => el(a1, a2, a3, a4, a5, a6, a7, a8))" call8_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2, a3, a4, a5, a6, a7, a8, a9) => el(a1, a2, a3, a4, a5, a6, a7, a8, a9))" call9_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((f, list) => f(...list))" callList_ :: JSVal -> JSVal -> IO JSVal

callList :: JSVal -> [JSVal] -> IO JSVal
callList f list = toJSArray list >>= callList_ f

call1 :: (ToJSVal a, ToJSVal b) => a -> b -> IO JSVal
call1 func arg1 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  call1_ func' arg1'

call2 :: (ToJSVal a, ToJSVal b, ToJSVal c) => a -> b -> c -> IO JSVal
call2 func arg1 arg2 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  call2_ func' arg1' arg2'

call3 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => a -> b -> c -> d -> IO JSVal
call3 func arg1 arg2 arg3 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  call3_ func' arg1' arg2' arg3'

call4 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => a -> b -> c -> d -> e -> IO JSVal
call4 func arg1 arg2 arg3 arg4 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  call4_ func' arg1' arg2' arg3' arg4'

call5 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f) => a -> b -> c -> d -> e -> f -> IO JSVal
call5 func arg1 arg2 arg3 arg4 arg5 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  arg5' <- toJSVal arg5
  call5_ func' arg1' arg2' arg3' arg4' arg5'

call6 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g) => a -> b -> c -> d -> e -> f -> g -> IO JSVal
call6 func arg1 arg2 arg3 arg4 arg5 arg6 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  arg5' <- toJSVal arg5
  arg6' <- toJSVal arg6
  call6_ func' arg1' arg2' arg3' arg4' arg5' arg6'

call7 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g, ToJSVal h) => a -> b -> c -> d -> e -> f -> g -> h -> IO JSVal
call7 func arg1 arg2 arg3 arg4 arg5 arg6 arg7 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  arg5' <- toJSVal arg5
  arg6' <- toJSVal arg6
  arg7' <- toJSVal arg7
  call7_ func' arg1' arg2' arg3' arg4' arg5' arg6' arg7'

call8 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g, ToJSVal h, ToJSVal i) => a -> b -> c -> d -> e -> f -> g -> h -> i -> IO JSVal
call8 func arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  arg5' <- toJSVal arg5
  arg6' <- toJSVal arg6
  arg7' <- toJSVal arg7
  arg8' <- toJSVal arg8
  call8_ func' arg1' arg2' arg3' arg4' arg5' arg6' arg7' arg8'

call9 :: (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g, ToJSVal h, ToJSVal i, ToJSVal j) => a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO JSVal
call9 func arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 = do
  func' <- toJSVal func
  arg1' <- toJSVal arg1
  arg2' <- toJSVal arg2
  arg3' <- toJSVal arg3
  arg4' <- toJSVal arg4
  arg5' <- toJSVal arg5
  arg6' <- toJSVal arg6
  arg7' <- toJSVal arg7
  arg8' <- toJSVal arg8
  arg9' <- toJSVal arg9
  call9_ func' arg1' arg2' arg3' arg4' arg5' arg6' arg7' arg8' arg9'

class ListBuilder r where
  build :: [JSVal] -> r

instance ListBuilder [JSVal] where
  build = reverse

instance (ListBuilder next_r) => ListBuilder (JSVal -> next_r) where
  build acc x = build (x : acc)

class MCompose f gresult result | f gresult -> result where
  mcomp :: f -> ([JSVal] -> gresult) -> result

instance MCompose (a -> [JSVal]) c (a -> c) where
  mcomp f g = g . f

instance (MCompose (f -> f') gresult result) => MCompose (a -> f -> f') gresult (a -> result) where
  mcomp f g a = mcomp (f a) g

call :: forall builder result. (ListBuilder builder, MCompose builder (IO JSVal) result) => result
call =
  (build [] :: builder) `mcomp` \case
    [] -> error "call requires at least one argument"
    (x : xs) -> callList x xs

(~>) :: forall builder result b. (ListBuilder builder, MCompose builder (IO JSVal) result, ToJSVal b) => JSVal -> b -> result
a ~> b = (build [] :: builder) `mcomp` ((a .> b >>=) . flip callList)

-- I still need to make it all work with ToJSVal

-- EXAMPLE:
--   print =<< call @(JSVal -> JSVal -> [JSVal]) 1 2

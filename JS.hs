-- Micro library for working with JS

module JS where

import Control.Arrow ((>>>))
import Control.Monad (void, when, (>=>))
import Data.String (IsString, fromString)
import GHC.JS.Prim (JSVal, fromJSInt, toJSInt, toJSString)

-- foreign import javascript "eval" eval :: JSVal -> IO JSVal

foreign import javascript "(()=>window)" window :: IO JSVal

foreign import javascript "(()=>document)" document :: IO JSVal

foreign import javascript "alert" alert_ :: JSVal -> IO ()

foreign import javascript "(()=>console)" console :: IO JSVal

foreign import javascript "document.getElementById" log_ :: JSVal -> IO JSVal

foreign import javascript "(()=>{debugger;})" debugger :: IO ()

foreign import javascript "((el, sel) => el[sel])" attr :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel) => el[sel]['bind'](el))" bound_attr :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel, val) => {el[sel] = val })" set_ :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "(el => el())" call0 :: JSVal -> IO JSVal

foreign import javascript "((el, a1) => el(a1))" call_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, a1, a2) => el(a1, a2))" call2_ :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((x) => {x})" jsFromInt :: Int -> IO JSVal

foreign import javascript "((x) => {x})" jsFromDouble :: Double -> IO JSVal

foreign import javascript "((fn) =>{\
   \const curried = (...args) => args.length >= fn.length ? fn(...args) : (...nextArgs) => curried(...args, ...nextArgs); \
   \  curried.fname = fn.name;\
   \  return curried;\
   \})"
  -- \  if (fn) {\
  -- \    Object.defineProperty(curried, 'name', { value: `curried_${fn.name}`, writable: false, })\
  -- \  };\
  jcurry_ :: JSVal -> IO JSVal

foreign import javascript "(fn => { if (typeof fn === 'function') {console.error(`Not enough arguments for function ${fn.fname} (${fn.name})`);} })" noignore :: JSVal -> IO JSVal

instance IsString JSVal where
  fromString = toJSString

(.>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a .> b = toJSVal b >>= attr a

(.>>) :: (ToJSVal b) => IO JSVal -> b -> IO JSVal
a .>> b = a >>= (.> b)

(.$>) :: (ToJSVal b) => JSVal -> b -> IO JSVal
a .$> b = toJSVal b >>= bound_attr a

(.$>>) :: (ToJSVal b) => IO JSVal -> b -> IO JSVal
a .$>> b = a >>= (.$> b)

alert :: (ToJSVal a) => a -> IO ()
alert a = toJSVal a >>= alert_

-- Apply result of jcurry to the next argument
($.) :: (ToJSVal a) => IO JSVal -> a -> IO JSVal
a $. b = a >>= llac b

llac :: (ToJSVal a) => (ToJSVal b) => a -> b -> IO JSVal
llac = flip call

jcurry :: (ToJSVal a) => IO a -> IO JSVal
jcurry val = val >>= toJSVal >>= jcurry_

jvoid :: IO JSVal -> IO () -- Works like void but says if you have tried to execute a function without enough elements and not just silently ignores the result
jvoid = (>>= noignore >>> void)

set :: (ToJSVal a) => (ToJSVal b) => JSVal -> b -> a -> IO ()
set obj key val = do
  k <- toJSVal key
  v <- toJSVal val
  void $ set_ obj k v

call :: (ToJSVal a) => (ToJSVal b) => a -> b -> IO JSVal
call a b = do
  a' <- toJSVal a
  b' <- toJSVal b
  call_ a' b'

call2 :: (ToJSVal a) => (ToJSVal b) => (ToJSVal c) => a -> b -> c -> IO JSVal
call2 a b c = do
  a' <- toJSVal a
  b' <- toJSVal b
  c' <- toJSVal c
  call2_ a' b' c'

class ToJSVal a where
  toJSVal :: a -> IO JSVal

instance ToJSVal Int where
  toJSVal = jsFromInt

instance ToJSVal Double where
  toJSVal = jsFromDouble

instance ToJSVal String where
  toJSVal = pure . toJSString

instance ToJSVal JSVal where
  toJSVal = pure

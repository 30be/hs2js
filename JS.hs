-- Micro library for working with JS
{-# LANGUAGE TemplateHaskell #-}

module JS where

import CallGen (jsFuncCalls)
import Control.Arrow ((>>>))
import Control.Monad (void, when, (>=>))
import Data.String (IsString, fromString)
import GHC.JS.Prim (JSVal, fromJSInt, toJSArray, toJSInt, toJSString)

-- foreign import javascript "eval" eval :: JSVal -> IO JSVal

foreign import javascript "(()=>window)" window :: IO JSVal

foreign import javascript "(()=>document)" document :: IO JSVal

foreign import javascript "alert" alert_ :: JSVal -> IO ()

foreign import javascript "(()=>console)" console :: IO JSVal

foreign import javascript "document.getElementById" log_ :: JSVal -> IO JSVal

foreign import javascript "(()=>{debugger;})" debugger :: IO ()

foreign import javascript "((el, sel) => ((typeof el[sel] === 'function') ? el[sel].bind(el) : el[sel]))" attr :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel, val) => {el[sel] = val })" set_ :: JSVal -> JSVal -> JSVal -> IO ()

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

alert :: (ToJSVal a) => a -> IO ()
alert a = toJSVal a >>= alert_

-- Apply result of jcurry to the next argument
($.) :: (ToJSVal a) => IO JSVal -> a -> IO JSVal
a $. b = a >>= llac b

llac :: (ToJSVal a) => (ToJSVal b) => a -> b -> IO JSVal
llac = flip call

jcurry :: (ToJSVal a) => IO a -> IO JSVal -- todo: merge that with jvoid with rewriting in the style of printf
jcurry val = val >>= toJSVal >>= jcurry_

jvoid :: IO JSVal -> IO () -- Works like void but says if you have tried to execute a function without enough elements and not just silently ignores the result
jvoid = (>>= noignore >>> void)

set :: (ToJSVal a) => (ToJSVal b) => JSVal -> b -> a -> IO ()
set obj key val = do
  k <- toJSVal key
  v <- toJSVal val
  void $ set_ obj k v

class ToJSVal a where
  toJSVal :: a -> IO JSVal

instance ToJSVal Int where
  toJSVal = pure . toJSString . show -- No idea how to do that better now

instance ToJSVal Double where
  toJSVal = pure . toJSString . show

instance ToJSVal String where
  toJSVal = pure . toJSString

instance ToJSVal JSVal where
  toJSVal = pure

instance ToJSVal [JSVal] where
  toJSVal = toJSArray

$(jsFuncCalls 9)

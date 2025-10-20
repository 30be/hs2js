-- Micro library for working with JS

module JS where

import Control.Arrow ((>>>))
import Control.Monad (void, when, (>=>))
import Data.String (IsString, fromString)
import GHC.JS.Prim (JSVal, toJSString)

foreign import javascript "eval" eval :: JSVal -> IO JSVal

foreign import javascript "(el, sel) => el[sel]" attr :: JSVal -> IO JSVal

foreign import javascript "(el => el())" call0 :: JSVal -> IO JSVal

foreign import javascript "((el, a1) => el(a1))" call :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((a1, el) => el(a1))" llac :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((fn) =>{\
   \const curried = (...args) => args.length >= fn.length ? fn(...args) : (...nextArgs) => curried(...args, ...nextArgs); \
   \  curried.fname = 'curried_' + fn.name;\
   \  return curried;\
   \})"
  -- \  if (fn) {\
  -- \    Object.defineProperty(curried, 'name', { value: `curried_${fn.name}`, writable: false, })\
  -- \  };\
  jcurry_ :: JSVal -> IO JSVal
foreign import javascript "(fn => { if (typeof fn === 'function') {console.error(`Not enough arguments for function ${fn.fname} (${fn.name})`);} })" noignore :: JSVal -> IO JSVal

instance IsString JSVal where
  fromString = toJSString

-- Apply result of jcurry to the next argument
($.) :: IO JSVal -> JSVal -> IO JSVal
a $. b = a >>= llac b

jcurry :: JSVal -> IO JSVal
jcurry = eval >=> jcurry_

jvoid :: IO JSVal -> IO () -- Works like void but says if you have tried to execute a function without enough elements and not just silently ignores the result
jvoid = (>>= noignore >>> void)

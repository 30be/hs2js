module JS where

import Control.Monad (void)
import GHC.JS.Prim (JSVal, toJSArray, toJSString)
import System.IO.Unsafe (unsafePerformIO)

foreign import javascript "(()=>window)" window :: IO JSVal

foreign import javascript "((el, sel) => ((typeof el[sel] === 'function') ? el[sel].bind(el) : el[sel]))" get :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((el, sel, val) => {el[sel] = val })" set_ :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "((obj) => {new obj()})" new :: JSVal -> IO JSVal

foreign import javascript "((f, args) => f(...args))" callList_ :: JSVal -> JSVal -> IO JSVal

(.>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a .> b = get a (toJSVal b)

(.>>) :: (ToJSVal b) => IO JSVal -> b -> IO JSVal
a .>> b = a >>= (.> b)

-- Call member function, like
-- window .>> "history" ~> "back"
(~>) :: (ToJSVal a) => JSVal -> a -> IO JSVal
a ~> b = a .> b >>= call

set :: (ToJSVal a) => (ToJSVal b) => JSVal -> b -> a -> IO ()
set obj key val = set_ obj (toJSVal key) (toJSVal val)

class ToJSVal a where
  toJSVal :: a -> JSVal

instance ToJSVal Int where
  toJSVal = toJSString . show -- TODO: Fix that

instance ToJSVal Double where
  toJSVal = toJSString . show

instance ToJSVal String where
  toJSVal = toJSString

instance ToJSVal JSVal where
  toJSVal = id

instance ToJSVal [JSVal] where
  toJSVal = unsafePerformIO . toJSArray

class CallReturn a where
  callReturn :: IO JSVal -> IO a

instance CallReturn JSVal where
  callReturn = id

instance CallReturn () where
  callReturn = void

class Call result where
  call' :: JSVal -> [JSVal] -> result

instance (CallReturn a) => Call (IO a) where
  call' f_jsval args = callReturn (callList f_jsval (reverse args))

instance (ToJSVal arg, Call result) => Call (arg -> result) where
  call' f_jsval args x = call' f_jsval (toJSVal x : args)

callList :: JSVal -> [JSVal] -> IO JSVal
callList f list = toJSArray list >>= callList_ f

-- Gets arguments first and the function last
call :: (Call result) => JSVal -> result
call f_jsval = call' f_jsval []

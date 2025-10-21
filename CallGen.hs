{-# LANGUAGE ScopedTypeVariables #-}
-- Fixed version of CallGen.hs
{-# LANGUAGE TemplateHaskell #-}

module CallGen (jsFuncCalls) where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Language.Haskell.TH

-- Type/Class Names
jsval_t, io_t, tojsval_c :: Name
jsval_t = mkName "JSVal"
io_t = mkName "IO"
tojsval_c = mkName "ToJSVal"

-- Generates both the FFI declaration and the Haskell wrapper for callN
genPair :: Int -> DecsQ
genPair n = do
  let ffiName = mkName $ "call" ++ show n ++ "_"
  let funcName = mkName $ "call" ++ show n

  -- 1. FFI Declaration Construction
  let jsArgs = "el" : map (\i -> 'a' : show i) [1 .. n]
  let jsBody = "el(" ++ intercalate ", " (tail jsArgs) ++ ")"
  let jsString = "((el, " ++ intercalate ", " (tail jsArgs) ++ ") => " ++ jsBody ++ ")"

  let jsValType = ConT jsval_t
  let ioJSValType = AppT (ConT io_t) jsValType
  let ffiTypeSig = foldr AppT ioJSValType (replicate (n + 1) jsValType)

  -- FIX: Reverting to the GHC 8.x FFI declaration structure that uses LitStr
  -- You may need to change 'JavaScript' to 'JavaScriptCall' depending on imports
  let ffiDecl = ForeignD (ImportF JavaScript Unsafe jsString ffiName ffiTypeSig)

  -- 2. Haskell Wrapper Construction
  argNames <- replicateM (n + 1) (newName "arg")
  typeVars <- replicateM (n + 1) (newName "t")
  primNames <- replicateM (n + 1) (newName "prim")

  let constraints = map (\t -> AppT (ConT tojsval_c) (VarT t)) typeVars
  let funcTypeSig = ForallT [] constraints (foldr AppT ioJSValType (map VarT typeVars))

  let bind t v = BindS (VarP t) (AppE (VarE $ mkName "toJSVal") (VarE v))
  let binds = zipWith bind primNames argNames
  let finalCall = NoBindS $ foldl AppE (VarE ffiName) (map VarE primNames)
  let bodyQ = DoE Nothing (binds ++ [finalCall])
  let funcDef = FunD funcName [Clause (map VarP argNames) (NormalB bodyQ) []]

  return [ffiDecl, funcDef]

jsFuncCalls :: Int -> DecsQ
jsFuncCalls maxArgs = fmap concat (mapM genPair [1 .. maxArgs])

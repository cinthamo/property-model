module Lib (f) where

import Language.ANTLR4
import Parser.PGrammar
import Parser.PParser
import Parser.Convert
import Text.Pretty.Simple (pPrint)

{- test -}
import Func.Static
import External.BasicTable
import Model.PropertiesMap
import Model.PropertiesObject
import Model.Resolvers.AspectResolver
import Model.Types
import Model.Definition
import Model.Value as V

f :: IO ()
f = do
        contents <- readFile "test.gxp"
        let x = case glrParse isWS contents of
                ResultAccept ast -> ast2definitions ast
                ResultSet x -> error $ "Multiple matches " ++ show x
                ErrorNoAction e _ _ -> error $ show e
        pPrint x
        pPrint $ map convert x
        test $ map convert x

test :: [DefinitionList] -> IO ()
test definitions = do
  print $ "typecheck " ++ show check
  print process1
  print process2
  print process3
  print process4
  print process5
  where
    obj = emptyWAsp $ head definitions
    check = typeCheck basicRef $ head definitions -- typeChecker test
    process1 = get basicRef obj "one" -- simple get
    process2 = get basicRef obj "two" -- default test
    process3 = static basicRef (set basicRef obj "one" (V.Number 1)) -- static, apply & readonly test
    process4 = get basicRef (set basicRef obj "four" (V.Number 1)) "four" -- valid test (invalid value)
    process5 = get basicRef (set basicRef obj "four" (V.Number 4)) "four" -- valid test (valid value)
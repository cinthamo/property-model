module Lib (f) where

import Language.ANTLR4
import Parser.PGrammar
import Parser.PParser
import Parser.Convert
import Text.Pretty.Simple (pPrint)

f :: IO ()
f = do
        contents <- readFile "test.gxp"
        let x = case glrParse isWS contents of
                ResultAccept ast -> ast2definitions ast
                ResultSet x -> error $ "Multiple matches " ++ show x
                ErrorNoAction e _ _ -> error $ show e
        pPrint x
        pPrint $ convert x

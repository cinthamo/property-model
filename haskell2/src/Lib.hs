module Lib (f) where

import Language.ANTLR4
import Gramma
import GParser

f :: IO ()
f = case glrParse isWS "//hola\n a(2)" of
        ResultAccept ast -> print $ ast2expr ast
        ErrorNoAction e _ _ -> error $ show e

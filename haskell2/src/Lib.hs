module Lib (f) where

import Language.ANTLR4
import Gramma
import GParser

f :: IO ()
f = case glrParse isWS "/*hola*/ nombre" of
        ResultAccept ast -> print $ ast2j ast
        ErrorNoAction e _ _ -> error $ show e

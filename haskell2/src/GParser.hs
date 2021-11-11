module GParser where

import Language.ANTLR4
import Gramma

$(g4_parsers testAST testGrammar)
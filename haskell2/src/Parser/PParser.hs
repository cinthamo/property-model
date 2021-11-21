module Parser.PParser where

import Language.ANTLR4
import Parser.PGrammar

$(g4_parsers testAST testGrammar)
module Parser.File (parseFile) where

import Language.ANTLR4
import Text.ANTLR.Pretty
import Parser.PGrammar
import Parser.PParser
import Parser.Convert
import Model.Definition

parseFile :: String -> IO [DefinitionList]
parseFile fileName = do
        contents <- readFile fileName
        return $ parse contents

parse :: String -> [DefinitionList]
parse s = map convert parsed
  where
    parsed = case glrParse isWS s of
      ResultAccept ast -> ast2definitions ast
      ResultSet x -> error $ "Multiple matches " ++ pshow' (ResultSet x)
      ErrorNoAction e _ _ -> error $ show e
module Runner.Interactive where

import Model.Definition

interactive :: [DefinitionList] -> IO ()
interactive d = do
    putStrLn "> "
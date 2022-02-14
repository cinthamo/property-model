module Checker.Check where

import Model.Definition
import External.BasicTable
import Checker.Types

check :: DefinitionList -> IO ()
check definitions =
    print $ typeCheck basicRef definitions
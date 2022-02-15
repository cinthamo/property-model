module Checker.Check where

import Model.Definition
import External.TypeTable
import Checker.Types

check :: DefinitionList -> IO ()
check definitions =
    print $ typeCheck typeTable definitions

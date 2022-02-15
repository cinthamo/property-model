module Checker.Check where

import Model.Definition
import External.TypeTable
import Checker.Types
import Checker.TypeContext

check :: DefinitionList -> IO ()
check definitions =
    print $ typeCheck (newTypeContext definitions functions externals) definitions

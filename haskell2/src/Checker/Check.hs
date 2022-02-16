module Checker.Check where

import Model.Definition
import External.TypeTable
import Checker.Types

check :: DefinitionList -> Bool
check definitions =
    typeCheck (newTypeContext definitions) definitions

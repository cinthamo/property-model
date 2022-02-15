module Checker.TypeTable where

import Data.Maybe
import Data.String.Interpolate ( i )
import Model.Value

data TypeTable
    = TypeTable {
        functions :: [(Name, [ValueType])]
    }

getForObj :: TypeTable -> Name -> ValueType
getForObj tt n = TNumber

getForProp :: TypeTable -> ValueType -> Name -> ValueType
getForProp tt obj prop = TNumber

getFuncParam :: TypeTable -> Name -> [ValueType]
getFuncParam tt n = case (lookup n $ functions tt) of
    Just x -> x
    Nothing -> error [i|"Function #{n} not found in table"|]

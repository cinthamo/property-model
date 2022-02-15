module External.TypeTable where

import Checker.TypeTable
import Model.Value

typeTable :: TypeTable
typeTable = TypeTable {
        functions = [("==", [TNumber, TNumber, TBool]),
                     ("+", [TNumber, TNumber, TNumber]),
                     (">", [TNumber, TNumber, TBool])]
    }

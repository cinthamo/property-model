module External.TypeTable where

import Model.Value
import Model.Definition
import Checker.TypeContext as TC

functions :: TFunctions
functions = [
        ("==", [TNumber, TNumber, TBool]),
        ("+", [TNumber, TNumber, TNumber]),
        (">", [TNumber, TNumber, TBool]),
        ("not", [TBool, TBool])
    ]

externals :: TExternals
externals = [
        (TExternal "WithIsInterface", TypeObject
            [
                ("isInterface", TBool)
            ] [

            ]),
        (TExternal "WithParent", TypeObject
            [
                ("parent", TInternal "AnotherObject")
            ] [

            ])
    ]

enums :: TEnums
enums = []

newTypeContext :: DefinitionList -> TypeContext
newTypeContext definitions = TC.newTypeContext definitions functions externals enums
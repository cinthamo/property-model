module External.TypeTable where

import Model.Value
import Model.Definition
import Checker.TypeContext as TC

functions :: TFunctions
functions = [
        ("==", [TGeneric 1, TGeneric 1, TBool]),
        ("<>", [TGeneric 1, TGeneric 1, TBool]),
        ("+", [TNumber, TNumber, TNumber]),
        ("-", [TNumber, TNumber, TNumber]),
        (">=", [TNumber, TNumber, TBool]),
        ("<=", [TNumber, TNumber, TBool]),
        (">", [TNumber, TNumber, TBool]),
        ("<", [TNumber, TNumber, TBool]),
        ("and", [TBool, TBool, TBool]),
        ("or", [TBool, TBool, TBool]),
        ("not", [TBool, TBool]),
        ("GetExposedName", [TString, TString]),
        ("GetName", [TExternal "LocalizableImageReference", TExternal "KBModel", TString])
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
                ("parent", TInternal "+WithParent")
            ] [

            ]),
        (TExternal "WithModel", TypeObject
            [
                ("model", TExternal "KBModel")
            ] [

            ]),
        (TExternal "WithContext", TypeObject
            [
                ("context", TExternal "RuntimeContext")
            ] [

            ])
    ]

enums :: TEnums
enums = [
        ("Country", ["Uruguay"]),
        ("RuntimeContext", ["Runtime", "Designtime"])
    ]

newTypeContext :: DefinitionList -> TypeContext
newTypeContext definitions = TC.newTypeContext definitions functions externals enums
module External.TypeTable where

import Model.Value
import Checker.TypeContext

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

            ])
    ]
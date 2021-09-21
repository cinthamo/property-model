module Test where

import Model.Definition
import Model.Value

definitions :: ObjectDefinition
definitions = ObjectDefinition {
    properties = [
        Definition "one" (num 3),
        Definition "two"
            (Case
                [
                    If
                        (Func "equal" [Ref "this" "one", num 1])                    
                        (num 2)
                ]
                (Func
                    "add" [
                        Func "add" [Ref "this" "one", (num (-1))],
                        (num 4)
                    ]
                )
            )
    ],
    related = []
}

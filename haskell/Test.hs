module Test where

import Model.Definition

definitions :: ObjectDefinition
definitions = ObjectDefinition {
    properties = [
        Definition "one" (Num 3),
        Definition "two"
            (Case
                [
                    If
                        (Func "equal" [Ref "one", Num 1])                    
                        (Num 2)
                ]
                (Func
                    "add" [
                        Func "add" [Ref "one", Num (-1)],
                        Num 4
                    ]
                )
            )
    ],
    related = []
}

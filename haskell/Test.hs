module Test where

import Model.Definition

definitions :: [Definition]
definitions = [
        Definition "one" "number" (Num 3),
        Definition "two" "number"
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
    ]

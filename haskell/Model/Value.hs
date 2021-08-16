module Model.Value where

import Model.Const

data Value obj = Data String |
                 Ref Name |
                 Obj obj |
                 List [Value obj]
    deriving Show
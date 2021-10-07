module Model.Value where

import Model.Const

data Value obj = String String |
                 Number Int |
                 Bool Bool |
                 Null |
                 Object obj |
                 List [Value obj]
    deriving Show
module Model.Value where

import Model.Const

data Value obj = String String |
                 Number Int |
                 Bool Bool |
                 Reference Name |
                 Object obj |
                 List [Value obj]
    deriving Show
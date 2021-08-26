module Model.Value where

import Model.Const
import Data.Scientific

data Value obj = String String |
                 Number Scientific |
                 Bool Bool |
                 Reference Name |
                 Object obj |
                 List [Value obj]
    deriving Show
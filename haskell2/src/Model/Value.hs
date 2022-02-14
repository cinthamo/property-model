module Model.Value where

import Model.Const

data Value obj = String String |
                 Number Int |
                 Bool Bool |
                 Null |
                 Object obj | -- Needed to have a properties object with its definition as a meta field
                 List [Value obj]
    deriving (Eq, Ord, Show)

data ValueType = TString |
                 TNumber |
                 TBool |
                 TUnknown | -- TODO: should not be needed
                 TExternal String
    deriving (Eq, Ord, Show)

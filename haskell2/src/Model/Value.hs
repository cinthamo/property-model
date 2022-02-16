module Model.Value where

type Name = String

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
                 TInternal String |
                 TExternal String |
                 TEnum String |
                 TEnumValue String
    deriving (Eq, Ord, Show)

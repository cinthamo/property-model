module Model.Property where

type Name = String
data Value obj = Data String |
                 Ref Name |
                 Obj obj

class PropertiesObject obj where
    get :: Name -> obj -> Maybe (Value obj)
    has :: Name -> obj -> Bool
    set :: Name -> Value obj -> obj -> obj
    clear :: Name -> obj -> obj

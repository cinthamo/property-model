module Model.Property where

type Name = String
data Value obj = Data String |
                 Ref Name |
                 Obj obj

class PropertiesObject obj where
    get :: Name -> obj -> Maybe (Value obj)
    set :: Name -> Value obj -> obj -> obj
    has :: Name -> obj -> Bool
    clear :: Name -> obj -> obj
    empty :: obj
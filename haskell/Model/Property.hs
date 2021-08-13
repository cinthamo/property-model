module Model.Property where

type Name = String

data Value obj = Data String |
                 Ref Name |
                 Obj obj
          
class PropertiesObject obj where
    get :: obj -> Name -> Maybe (Value obj)
    has :: obj -> Name -> Bool
    set :: obj -> Name -> Value obj -> obj
    clear :: obj -> Name -> obj
    empty :: obj

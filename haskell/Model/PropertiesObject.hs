module Model.PropertiesObject where

type Name = String

data Value obj = Data String |
                 Ref Name |
                 Obj obj
          
class PropertiesObject obj where
    has :: obj -> Name -> Bool
    get :: obj -> Name -> Maybe (Value obj)
    set :: obj -> Name -> Value obj -> obj
    clear :: obj -> Name -> obj
    empty :: obj

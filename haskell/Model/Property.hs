module Model.Property where

import Model.Property
import Model.Resolver

type Name = String
data Value obj = Data String |
                 Ref Name |
                 Obj obj |
                 Res Resolver |
                 Lis Listener

class PropertiesObject obj where
    get :: Name -> obj -> Maybe (Value obj)
    has :: Name -> obj -> Bool
    set :: Name -> Value obj -> obj -> obj
    clear :: Name -> obj -> obj
    empty :: obj

module Model.PropertiesObject where

import Model.Const
import Model.Resolver

data Behaviour obj = BEmpty | BResolver (Resolver obj)

class PropertiesObject obj where
    has :: obj -> obj -> Name -> Bool
    get :: obj -> obj -> Name -> Maybe (Value obj)
    set :: obj -> obj -> Name -> Value obj -> obj
    clear :: obj -> obj -> Name -> obj
    empty :: Behaviour obj -> obj
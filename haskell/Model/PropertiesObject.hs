module Model.PropertiesObject where

import Model.Const
import Model.Resolver
import Model.Value
import Data.Map

data Behaviour obj = BEmpty | BResolver (Resolver obj)

class PropertiesObject obj where
    has :: RefTable obj -> obj -> Name -> Bool
    get :: RefTable obj -> obj -> Name -> Maybe (Value obj)
    set :: RefTable obj -> obj -> Name -> Value obj -> obj
    clear :: RefTable obj -> obj -> Name -> obj
    empty :: Behaviour obj -> obj
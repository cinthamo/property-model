module Runner.PropertiesObject where

import Model.Definition
import Model.Value
import Runner.Const
import Runner.Resolvers.Resolver

data Behaviour obj = BEmpty | BResolver (Resolver obj)

class PropertiesObject obj where
    all :: RefTable obj -> obj -> [Name]
    has :: RefTable obj -> obj -> Name -> Bool
    get :: RefTable obj -> obj -> Name -> Maybe (Value obj)
    set :: RefTable obj -> obj -> Name -> Value obj -> obj
    clear :: RefTable obj -> obj -> Name -> obj
    empty :: Behaviour obj -> DefinitionList -> obj
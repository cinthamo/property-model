module Runner.PropertiesInfo where

import Model.Value
import Runner.Resolvers.Resolver

class PropertiesInfo obj where
    isReadonly :: RefTable obj -> obj -> Name -> Bool
    isDefault :: obj -> Name -> Bool
    getDoc :: obj -> Name -> String

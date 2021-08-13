module Model.Resolver where

import Model.Property

data ResolveGet value = GNotResolved | GResolved value
data ResolveBeforeSet value = BSNotResolved | BSInvalid | BSValue value
data ResolveAfterSet = ASNotResolved | ASResolved

data Resolver obj = Resolver
    { beforeHas :: obj -> Name -> ResolveGet Bool
    , afterHas :: obj -> Name -> Bool -> ResolveGet Bool
    , beforeGet :: obj -> Name -> ResolveGet (Value obj)
    , afterGet :: obj -> Name -> Maybe (Value obj) -> ResolveGet (Value obj)
    }
    {-- beforeSet
    (PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveBeforeSet obj)
    -- afterSet
    (PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveAfterSet)
    -- beforeClear
    (PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveBeforeSet obj)
    -- afterClear
    (PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveAfterSet)
    -}

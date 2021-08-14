module Model.Resolver where

import Model.Property

data ResolveGet value = GNotResolved | GResolved value
data ResolveBeforeSet value = BSNotResolved | BSCancel | BSValue value
data ResolveAfterSet = ASNotResolved | ASResolved
data Void

data Resolver obj = Resolver
    { beforeHas :: obj -> Name -> ResolveGet Bool
    , afterHas :: obj -> Name -> Bool -> ResolveGet Bool
    , beforeGet :: obj -> Name -> ResolveGet (Value obj)
    , afterGet :: obj -> Name -> Maybe (Value obj) -> ResolveGet (Value obj)
    , beforeSet :: obj -> Name -> Value obj -> ResolveBeforeSet (Value obj)
    , afterSet :: obj -> Name -> Value obj -> ResolveAfterSet
    , beforeClear :: obj -> Name -> ResolveBeforeSet Void
    , afterClear :: obj -> Name -> ResolveAfterSet
    }

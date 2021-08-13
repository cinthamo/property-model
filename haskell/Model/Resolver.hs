module Model.Resolver where

import Model.Property

data ResolveGet value = GNotResolved | GResolved value
data ResolveBeforeSet value = BSNotResolved | BSInvalid | BSValue value
data ResolveAfterSet = ASNotResolved | ASResolved

class Resolver r where
    beforeGet :: PropertiesObject obj => obj -> Name -> r -> ResolveGet (Value obj)
    afterGet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveGet (Value obj)
    beforeHas :: PropertiesObject obj => obj -> Name -> r -> ResolveGet Bool
    afterHas :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveGet Bool
    beforeSet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveBeforeSet obj
    aftereSet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveAfterSet
    beforeClear :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveBeforeSet obj
    afterClear :: PropertiesObject obj => obj -> Name -> Value obj -> r -> ResolveAfterSet

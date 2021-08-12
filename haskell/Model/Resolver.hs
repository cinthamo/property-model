module Model.Resolver where

import Model.Property

type GetValueResult value = (Bool, Maybe value)
type BeforeSetValueResult value = (Bool, Maybe Bool, Maybe value)
type AfterSetValueResult = Bool

class Resolver r where
    beforeGet :: PropertiesObject obj => obj -> Name -> r -> obj -> GetValueResult (Value obj)
    afterGet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> GetValueResult (Value obj)
    beforeSet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> BeforeSetValueResult obj
    aftereSet :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> AfterSetValueResult
    beforeHas :: PropertiesObject obj => obj -> Name -> r -> obj -> GetValueResult Bool
    afterHas :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> GetValueResult Bool
    beforeClear :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> BeforeSetValueResult obj
    afterClear :: PropertiesObject obj => obj -> Name -> Value obj -> r -> obj -> AfterSetValueResult

module Model.Behaviour where

import Model.PropertiesObject
import Model.Resolver
import Model.EmptyResolver

resolver :: PropertiesObject obj => Behaviour obj -> Resolver obj
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

emptyObj :: PropertiesObject obj => obj
emptyObj = empty BEmpty
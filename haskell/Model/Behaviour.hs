module Model.Behaviour where

import Model.PropertiesObject
import Model.Resolvers.Resolver
import Model.Resolvers.EmptyResolver
import Debug.Trace

resolver :: PropertiesObject obj => Behaviour obj -> Resolver obj
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

emptyObj :: PropertiesObject obj => obj
emptyObj = empty BEmpty
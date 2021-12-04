module Model.Behaviour where

import Model.Definition
import Model.PropertiesObject
import Model.Resolvers.Resolver
import Model.Resolvers.EmptyResolver
import Debug.Trace

resolver :: PropertiesObject obj => Behaviour obj -> Resolver obj
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

emptyDefinition :: DefinitionList
emptyDefinition = DefinitionList {
    lname = "",
    properties = [],
    related = []
}

emptyObj :: PropertiesObject obj => obj
emptyObj = empty BEmpty emptyDefinition
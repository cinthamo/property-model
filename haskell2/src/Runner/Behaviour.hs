module Runner.Behaviour where

import Model.Definition
import Runner.PropertiesObject
import Runner.Resolvers.Resolver
import Runner.Resolvers.EmptyResolver
import Debug.Trace

resolver :: PropertiesObject obj => Behaviour obj -> Resolver obj
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

emptyDefinition :: DefinitionList
emptyDefinition = DefinitionList {
    lname = "",
    externalType = Nothing,
    properties = []
}

emptyObj :: PropertiesObject obj => obj
emptyObj = empty BEmpty emptyDefinition
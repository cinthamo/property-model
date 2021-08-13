module Model.PropertyMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Model.Property
import Model.Resolver

data PropertiesMap = PM (Map Name (Value (PropertiesMap)))

type PropertyGet = Name -> PropertiesMap -> Maybe (Value (PropertiesMap))

--- Properties as Bag ---

getAsBag:: PropertyGet
getAsBag name (PM b map) = Map.lookup name map

setAsBag :: Name -> Value PropertiesMap -> PropertiesMap -> PropertiesMap
setAsBag name value (PM b map) = PM b (Map.insert name value map)

hasAsBag :: Name -> PropertiesMap -> Bool
hasAsBag name (PM b map) = Map.member name map

clearAsBag :: Name -> PropertiesMap -> PropertiesMap
clearAsBag name (PM b map) = PM b (Map.delete name map)

--- Properties with Resolvers ---

getContext :: behaviour -> PropertiesMap -> PropertiesMap
getContext b o = set "instance" (Obj o) (emptyMap b)

getWithResolver:: Resolver behaviour => PropertyGet -> PropertiesMap -> PropertyGet
getWithResolver valueGet context = \name obj@(PM resolver map) ->
        case (beforeGet context name resolver) of
            GNotResolved -> Nothing
            GResolved x -> Just x


        {-case (beforeGet context name resolver) of
            Just x -> x
            Nothing -> let value = valueGet name map obj in
                case (afterGet context name value resolver) of
                    Just y -> y
                    Nothing -> (\n o -> value)-}

--- Properties with Listener ---


--- PropertiesMap Instance ---

instance PropertiesObject PropertiesMap where
    get name obj = getWithResolver getAsBag (getContext "" obj) name obj
    set = setAsBag
    has = hasAsBag
    clear = clearAsBag
    emptyMap = PM Map.empty

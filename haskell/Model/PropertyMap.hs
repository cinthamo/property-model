module Model.PropertyMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Model.Property
import Model.Resolver

data PropertiesMap behaviour = PM behaviour (Map Name (Value (PropertiesMap behaviour)))

type PropertyGet behaviour = Name -> PropertiesMap behaviour -> Maybe (Value (PropertiesMap behaviour))

--- Properties as Bag ---

getAsBag:: PropertyGet behaviour
getAsBag name (PM b map) = Map.lookup name map

setAsBag :: Name -> Value (PropertiesMap behaviour) -> PropertiesMap behaviour -> PropertiesMap behaviour
setAsBag name value (PM b map) = PM b (Map.insert name value map)

hasAsBag :: Name -> PropertiesMap behaviour -> Bool
hasAsBag name (PM b map) = Map.member name map

clearAsBag :: Name -> PropertiesMap behaviour -> PropertiesMap behaviour
clearAsBag name (PM b map) = PM b (Map.delete name map)

--- Properties with Resolvers ---

getContext :: behaviour -> PropertiesMap instanceBehaviour -> PropertiesMap behaviour
getContext b o = set "instance" (Obj o) (emptyMap b)

getWithResolver:: Resolver behaviour => PropertyGet behaviour -> PropertiesMap behaviour -> PropertyGet behaviour
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

emptyMap :: behaviour -> PropertiesMap behaviour
emptyMap b = PM b Map.empty

instance PropertiesObject (PropertiesMap behaviour) where
    get name obj = getWithResolver getAsBag (getContext "" obj) name obj
    set = setAsBag
    has = hasAsBag
    clear = clearAsBag

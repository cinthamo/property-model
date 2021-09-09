module Model.PropertiesMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Model.Const
import Model.PropertiesObject
import Model.Behaviour
import Model.Context
import Model.Value
import Model.Resolvers.Resolver
import Debug.Trace

data PropertiesMap = PM (Behaviour PropertiesMap) (Map Name (Value PropertiesMap))

type PropertyAll = PropertiesMap -> [Name]
type PropertyHas = PropertiesMap -> Name -> Bool
type PropertyGet = PropertiesMap -> Name -> Maybe (Value PropertiesMap)
type PropertySet = PropertiesMap -> Name -> Value PropertiesMap -> PropertiesMap
type PropertyClear = PropertiesMap -> Name -> PropertiesMap

--- Properties as Bag ---

allAsBag :: PropertyAll
allAsBag (PM _ map) = Map.keys map

hasAsBag :: PropertyHas
hasAsBag (PM _ map) name = Map.member name map

getAsBag:: PropertyGet
getAsBag (PM _ map) name = Map.lookup name map

setAsBag :: PropertySet
setAsBag (PM b map) name value = PM b (Map.insert name value map)

clearAsBag :: PropertyClear
clearAsBag (PM b map) name = PM b (Map.delete name map)

--- Properties with Resolvers ---

allWithResolver :: PropertyAll -> Context PropertiesMap -> PropertyAll
allWithResolver allHas context = \obj@(PM behaviour map) ->
    let r = resolver behaviour
        list = allHas obj
    in (getAll r) context list

hasWithResolver :: PropertyHas -> Context PropertiesMap -> PropertyHas
hasWithResolver valueHas context = \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeHas r) context) of
        GResolved x -> x
        GNotResolved ->
            let hasIt = valueHas obj name
            in case ((afterHas r) context hasIt) of
                GResolved x -> x
                GNotResolved -> hasIt

getWithResolver :: PropertyGet -> Context PropertiesMap -> PropertyGet
getWithResolver valueGet context = \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeGet r) context) of
        GResolved x -> Just x
        GNotResolved ->
            let value = valueGet obj name
            in case ((afterGet r) context value) of
                GResolved x -> Just x
                GNotResolved -> value

setWithResolver :: PropertySet -> Context PropertiesMap -> PropertySet
setWithResolver valueSet context = \obj@(PM behaviour map) name value ->
    let r = resolver behaviour
    in case ((beforeSet r) context value) of
        BSCancel -> obj
        BSValue x -> continue obj name x r
        BSNotResolved -> continue obj name value r
        where
            continue = \obj name value r ->
                let newObj = valueSet obj name value
                    newContext = getContextWithInstance context newObj
                    _ = ((afterSet r) newContext value)
                in newObj

clearWithResolver :: PropertyClear -> Context PropertiesMap -> PropertyClear
clearWithResolver valueClear context = \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeClear r) context) of
        BSCancel -> obj
        BSNotResolved ->
            let newObj = valueClear obj name
                _ = ((afterClear r) (getContextWithInstance context newObj))
            in newObj

--- PropertiesMap Instance ---

instance PropertiesObject PropertiesMap where
    all   refTable obj      = allWithResolver   allAsBag   (getContext refTable obj "") obj 
    has   refTable obj name = hasWithResolver   hasAsBag   (getContext refTable obj name) obj name
    get   refTable obj name = getWithResolver   getAsBag   (getContext refTable obj name) obj name
    set   refTable obj name = setWithResolver   setAsBag   (getContext refTable obj name) obj name
    clear refTable obj name = clearWithResolver clearAsBag (getContext refTable obj name) obj name
    empty behaviour         = PM behaviour Map.empty

instance Show PropertiesMap where
    show (PM behaviour map) = show . Map.toList $ map
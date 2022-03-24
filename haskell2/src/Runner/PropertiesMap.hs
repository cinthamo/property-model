module Runner.PropertiesMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Debug.Trace
import Model.Definition as D
import Model.Value
import Runner.Const
import Runner.PropertiesObject
import Runner.PropertiesInfo
import Runner.Behaviour
import Runner.Context
import Runner.Resolvers.Resolver
import Runner.Resolvers.ReadonlyResolver as RR

data PropertiesMap = PM (Behaviour PropertiesMap) DefinitionList (Map Name (Value PropertiesMap))

type PropertyAll = PropertiesMap -> [Name]
type PropertyHas = PropertiesMap -> Name -> Bool
type PropertyGet = PropertiesMap -> Name -> Maybe (Value PropertiesMap)
type PropertySet = PropertiesMap -> Name -> Value PropertiesMap -> PropertiesMap
type PropertyClear = PropertiesMap -> Name -> PropertiesMap

--- Properties as Bag ---

allAsBag :: PropertyAll
allAsBag (PM _ _ map) = Map.keys map

hasAsBag :: PropertyHas
hasAsBag (PM _ _ map) name = Map.member name map

getAsBag:: PropertyGet
getAsBag (PM _ _ map) name = Map.lookup name map

setAsBag :: PropertySet
setAsBag (PM b d map) name value = PM b d (Map.insert name value map)

clearAsBag :: PropertyClear
clearAsBag (PM b d map) name = PM b d (Map.delete name map)

--- Properties with Resolvers ---

allWithResolver :: PropertyAll -> Context PropertiesMap -> PropertyAll
allWithResolver allHas context = \obj@(PM behaviour definitions map) ->
    let list = allHas obj
        defined = Data.List.map D.name (properties definitions)
        all = nub (list ++ defined)
    in filter (has (refTable context) obj) all

hasWithResolver :: PropertyHas -> Context PropertiesMap -> PropertyHas
hasWithResolver valueHas context = \obj@(PM behaviour _ map) name ->
    let r = resolver behaviour
    in case ((beforeHas r) context) of
        GResolved x -> x
        GNotResolved ->
            let hasIt = valueHas obj name
            in case ((afterHas r) context hasIt) of
                GResolved x -> x
                GNotResolved -> hasIt

getWithResolver :: PropertyGet -> Context PropertiesMap -> PropertyGet
getWithResolver valueGet context = \obj@(PM behaviour _ map) name ->
    let r = resolver behaviour
    in case ((beforeGet r) context) of
        GResolved x -> Just x
        GNotResolved ->
            let value = valueGet obj name
            in case ((afterGet r) context value) of
                GResolved x -> Just x
                GNotResolved -> value

setWithResolver :: PropertySet -> Context PropertiesMap -> PropertySet
setWithResolver valueSet context = \obj@(PM behaviour _ map) name value ->
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
clearWithResolver valueClear context = \obj@(PM behaviour _ map) name ->
    let r = resolver behaviour
    in case ((beforeClear r) context) of
        BSCancel -> obj
        BSNotResolved ->
            let newObj = valueClear obj name
                _ = ((afterClear r) (getContextWithInstance context newObj))
            in newObj

--- PropertiesMap Instance ---

instance PropertiesObject PropertiesMap where
    all   refTable obj@(PM _ definitions _)      = allWithResolver   allAsBag   (getContext refTable obj definitions "") obj 
    has   refTable obj@(PM _ definitions _) name = hasWithResolver   hasAsBag   (getContext refTable obj definitions name) obj name
    get   refTable obj@(PM _ definitions _) name = getWithResolver   getAsBag   (getContext refTable obj definitions name) obj name
    set   refTable obj@(PM _ definitions _) name = setWithResolver   setAsBag   (getContext refTable obj definitions name) obj name
    clear refTable obj@(PM _ definitions _) name = clearWithResolver clearAsBag (getContext refTable obj definitions name) obj name
    empty behaviour definitions                  = PM behaviour definitions Map.empty

instance PropertiesInfo PropertiesMap where
    isReadonly refTable obj@(PM _ definitions _) name = RR.isReadonly (getContext refTable obj definitions name)
    isDefault (PM _ _ map) name = not $ Map.member name map
    getDoc (PM _ definitions _) n = case (find (\x -> D.name x == n) (properties definitions)) of
                        Just y -> intercalate "\n" $ doc y
                        _ -> ""

instance Show PropertiesMap where
    show (PM _ _ map) = show . Map.toList $ map

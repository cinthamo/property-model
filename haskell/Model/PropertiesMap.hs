module Model.PropertiesMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Model.PropertiesObject
import Model.Resolver

data Behaviour = BEmpty | BResolver (Resolver PropertiesMap)
data PropertiesMap = PM Behaviour (Map Name (Value PropertiesMap))

type PropertyHas = PropertiesMap -> Name -> Bool
type PropertyGet = PropertiesMap -> Name -> Maybe (Value PropertiesMap)
type PropertySet = PropertiesMap -> Name -> Value PropertiesMap -> PropertiesMap
type PropertyClear = PropertiesMap -> Name -> PropertiesMap

--- Properties as Bag ---

hasAsBag :: PropertyHas
hasAsBag (PM b map) name = Map.member name map

getAsBag:: PropertyGet
getAsBag (PM b map) name = Map.lookup name map

setAsBag :: PropertySet
setAsBag (PM b map) name value = PM b (Map.insert name value map)

clearAsBag :: PropertyClear
clearAsBag (PM b map) name = PM b (Map.delete name map)

--- Properties with Resolvers ---

emptyResolver :: Resolver PropertiesMap
emptyResolver = Resolver
    (\x y -> GNotResolved)
    (\x y z -> GNotResolved)
    (\x y -> GNotResolved)
    (\x y z -> GNotResolved)
    (\x y z -> BSNotResolved)
    (\x y z -> ASNotResolved)
    (\x y -> BSNotResolved)
    (\x y -> ASNotResolved)

resolver :: Behaviour -> Resolver PropertiesMap
resolver BEmpty = emptyResolver
resolver (BResolver r) = r

hasWithResolver :: PropertyHas -> PropertiesMap -> PropertyHas
hasWithResolver valueHas context = \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeHas r) context name) of
        GResolved x -> x
        GNotResolved ->
            let hasIt = valueHas obj name
            in case ((afterHas r) context name hasIt) of
                GResolved x -> x
                GNotResolved -> hasIt

getWithResolver :: PropertyGet -> PropertiesMap -> PropertyGet
getWithResolver valueGet context = \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeGet r) context name) of
        GResolved x -> Just x
        GNotResolved ->
            let value = valueGet obj name
            in case ((afterGet r) context name value) of
                GResolved x -> Just x
                GNotResolved -> value

setWithResolver :: PropertySet -> PropertiesMap -> PropertySet
setWithResolver valueSet context = \obj@(PM behaviour map) name value ->
    let r = resolver behaviour
    in case ((beforeSet r) context name value) of
        BSCancel -> obj
        BSValue x -> continue obj name x r
        BSNotResolved -> continue obj name value r
        where
            continue = \obj name value r ->
                let newObj = valueSet obj name value
                    _ = ((afterSet r) (getContext newObj name) name value)
                in newObj

clearWithResolver :: PropertyClear -> PropertiesMap -> PropertyClear
clearWithResolver valueClear context =  \obj@(PM behaviour map) name ->
    let r = resolver behaviour
    in case ((beforeClear r) context name) of
        BSCancel -> obj
        BSNotResolved ->
            let newObj = valueClear obj name
                _ = ((afterClear r) (getContext newObj name) name)
            in newObj

--- Definition ---

getDefinition :: PropertiesObject obj => obj -> Name -> Maybe obj
getDefinition obj name =
    case (get obj "@definitions") of
        Just (Obj definitions) ->
            case (get definitions name) of
                Just (Obj x) -> Just x
                _ -> Nothing
        _ -> Nothing

--- Context ---

getContext :: PropertiesObject obj => obj -> Name -> obj
getContext i name = 
    let context = set empty "instance" (Obj i)
    in case (getDefinition i name) of
        Nothing -> context
        Just x -> set context "definition" (Obj x)

--- PropertiesMap Instance ---

instance PropertiesObject PropertiesMap where
    has obj name = hasWithResolver hasAsBag (getContext obj name) obj name
    get obj name = getWithResolver getAsBag (getContext obj name) obj name
    set obj name = setWithResolver setAsBag (getContext obj name) obj name
    clear obj name = clearWithResolver clearAsBag (getContext obj name) obj name
    empty = PM BEmpty Map.empty

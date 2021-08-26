module Model.Context where

import Model.PropertiesObject
import Model.Resolver
import Model.Const
import Model.Behaviour
import Model.EmptyResolver
import Model.Value
import Data.Map as M
import Debug.Trace

getContext :: PropertiesObject obj => RefTable obj -> obj -> Name -> Context obj
getContext refTable i name = Context {
        refTable = refTable,
        objects = objects,
        name = name
    }
    where
        objects =
            if (name == cMETA_DEFINITIONS) then
                fromList [(cINSTANCE, i)]
            else case (getDefinition i name) of
                Nothing -> fromList [(cINSTANCE, i)]
                Just d -> fromList [(cINSTANCE, i), (cDEFINITION, d)]

getContextWithInstance :: PropertiesObject obj => Context obj -> obj -> Context obj
getContextWithInstance oldContext i = Context {
        refTable = refTable oldContext,
        objects = insert cINSTANCE i (objects oldContext),
        name = name oldContext
    }

--- Getters ---

-- get object from context
getContextObj :: PropertiesObject obj => Context obj -> Name -> Maybe obj
getContextObj context name = M.lookup name (objects context)

-- get object resolving references
getObject :: PropertiesObject obj => RefTable obj -> Value obj -> obj
getObject refTable value =
    case (value) of
        Object x -> x
        Reference refName ->
            case (M.lookup refName refTable) of
                Just (RefObj y) -> y
                _ -> emptyObj
        _ -> emptyObj

-- get resolver resolving reference
getResolver :: PropertiesObject obj => RefTable obj -> Value obj -> Resolver obj
getResolver refTable value =
    case (value) of
        Reference refName ->
            case (M.lookup refName refTable) of
                Just (RefRes y) -> y
                _ -> emptyResolver
        _ -> emptyResolver

-- get function from reference
getFunction :: RefTable obj -> Name -> Function obj
getFunction refTable name =
    case (M.lookup name refTable) of
        Just (RefFunc f) -> f
        _ -> error $ "unknown function " ++ name

-- get value from definition using context
getDefinitionValue :: PropertiesObject obj => Context obj -> Name -> Maybe (Value obj)
getDefinitionValue context name =
    case (getContextObj context cDEFINITION) of
        Just definition -> get (refTable context) definition name
        _ -> Nothing

-- get instance from context
getInstance :: PropertiesObject obj => Context obj -> obj
getInstance context =
    case (getContextObj context cINSTANCE) of
        Just i -> i
        _ -> error "it should have the instance"

--- Definition ---

getDefinition :: PropertiesObject obj => obj -> Name -> Maybe obj
getDefinition obj name =
    case (get M.empty obj cMETA_DEFINITIONS) of
        Just (Object definitions) ->
            case (get M.empty definitions name) of
                Just (Object x) -> Just x
                _ -> Nothing
        _ -> Nothing

--- Type ---

getType :: PropertiesObject obj => obj -> Name
getType obj = case (get M.empty obj cTYPE) of
    Just (Reference t) -> t
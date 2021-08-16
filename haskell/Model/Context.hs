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
getContextObj :: PropertiesObject obj => Context obj -> Name -> obj
getContextObj context name =
    case (M.lookup name (objects context)) of
        Just x -> x
        _ -> emptyObj

-- get object resolving references
getObject :: PropertiesObject obj => RefTable obj -> Value obj -> obj
getObject refTable value =
    case (value) of
        Obj x -> x
        Ref refName ->
            case (M.lookup refName refTable) of
                Just (RefObj y) -> y
                _ -> emptyObj
        _ -> emptyObj

-- get resolver from context value resolving references
getResolver :: PropertiesObject obj => RefTable obj -> Value obj -> Resolver obj
getResolver refTable value =
    case (value) of
        Ref refName ->
            case (M.lookup refName refTable) of
                Just (RefRes y) -> y
                _ -> emptyResolver
        _ -> emptyResolver

--- Definition ---

getDefinition :: PropertiesObject obj => obj -> Name -> Maybe obj
getDefinition obj name =
    case (get M.empty obj cMETA_DEFINITIONS) of
        Just (Obj definitions) ->
            case (get M.empty definitions name) of
                Just (Obj x) -> Just x
                _ -> Nothing
        _ -> Nothing
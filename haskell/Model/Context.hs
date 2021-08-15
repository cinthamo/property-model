module Model.Context where

import Model.PropertiesObject
import Model.Resolver
import Model.Const
import Model.Behaviour
import Model.EmptyResolver

getContext :: PropertiesObject obj => obj -> obj -> Name -> obj
getContext refTable i name = 
    let obj1    = set emptyObj emptyObj cREF_TABLE (Obj refTable)
        obj2    = set emptyObj obj1     cNAME      (Ref name)
        context = set emptyObj obj2     cINSTANCE  (Obj i)
    in case (getDefinition refTable i name) of
        Nothing -> context
        Just x -> set refTable context cDEFINITION (Obj x)

getContextWithInstance :: PropertiesObject obj => obj -> obj -> obj
getContextWithInstance oldContext i = set emptyObj oldContext cINSTANCE (Obj i)

--- Getters ---

-- get object from context
getContextObj :: PropertiesObject obj => obj -> Name -> obj
getContextObj context name =
    case (get emptyObj context name) of
        Just (Obj x) -> x
        _ -> emptyObj

-- get name from context
getContextName :: PropertiesObject obj => obj -> Name
getContextName context =
    case (get emptyObj context cNAME) of
        Just (Ref n) -> n
        _ -> ""

-- get object from context value resolving references
getObject :: PropertiesObject obj => obj -> Value obj -> obj
getObject context value =
    case (value) of
        Obj x -> x
        Ref refName -> 
            let refTable = getContextObj context cREF_TABLE
            in case (get emptyObj refTable refName) of
                Just (Obj y) -> y
                _ -> emptyObj
        _ -> emptyObj

-- get resolver from context value resolving references
getResolver :: PropertiesObject obj => obj -> (Value obj) -> Resolver obj
getResolver context value =
    case (value) of
        Res x -> x
        Ref refName ->
            let refTable = getContextObj context cREF_TABLE
            in case (get emptyObj refTable refName) of
                Just (Res y) -> y
                _ -> emptyResolver
        _ -> emptyResolver

--- Definition ---

getDefinition :: PropertiesObject obj => obj -> obj -> Name -> Maybe obj
getDefinition refTable obj name =
    case (get refTable obj cMETA_DEFINITIONS) of
        Just (Obj definitions) ->
            case (get refTable definitions name) of
                Just (Obj x) -> Just x
                _ -> Nothing
        _ -> Nothing
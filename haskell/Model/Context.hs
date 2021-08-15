module Model.Context where

import Model.PropertiesObject
import Model.Resolver

getContext :: PropertiesObject obj => obj -> obj -> Name -> obj
getContext refTable i name = 
    let obj1 = set refTable (empty BEmpty) "refTable" (Obj refTable)
        context = set refTable obj1 "instance" (Obj i)
    in case (getDefinition refTable i name) of
        Nothing -> context
        Just x -> set refTable context "definition" (Obj x)

getContextWithInstance :: PropertiesObject obj => obj -> obj -> obj
getContextWithInstance oldContext i = set (empty BEmpty) oldContext "instance" (Obj i)

--- Getters ---

-- get object from context
getContextObj :: PropertiesObject obj => obj -> Name -> obj
getContextObj context name =
    case (get (empty BEmpty) context name) of
        Just (Obj x) -> x
        _ -> empty (BEmpty)

-- get object from context value resolving references
getObject :: PropertiesObject obj => obj -> Name -> Name -> Maybe obj
getObject context contextName propertyName =
    let obj = getContextObj context contextName
    in case (get (empty BEmpty) obj propertyName) of
        Just (Obj x) -> Just x
        Just (Ref refName) -> 
            let refTable = getContextObj context "refTable"
            in case (get (empty BEmpty) refTable refName) of
                Just (Obj y) -> Just y
                _ -> Nothing
        _ -> Nothing

-- get resolver from context value resolving references
getResolver :: PropertiesObject obj => obj -> Name -> Name -> Maybe (Resolver obj)
getResolver context contextName propertyName =
    let obj = getContextObj context contextName
    in case (get (empty BEmpty) obj propertyName) of
        Just (Res x) -> Just x
        Just (Ref refName) -> 
            let refTable = getContextObj context "refTable"
            in case (get (empty BEmpty) refTable refName) of
                Just (Res y) -> Just y
                _ -> Nothing
        _ -> Nothing

--- Definition ---

getDefinition :: PropertiesObject obj => obj -> obj -> Name -> Maybe obj
getDefinition refTable obj name =
    case (get refTable obj "@definitions") of
        Just (Obj definitions) ->
            case (get refTable definitions name) of
                Just (Obj x) -> Just x
                _ -> Nothing
        _ -> Nothing
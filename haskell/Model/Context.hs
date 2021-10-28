module Model.Context where

import Model.PropertiesObject as PO
import Model.Const
import Model.Behaviour
import Model.Definition as D
import Model.Value
import Model.Resolvers.Resolver as R
import Model.Resolvers.EmptyResolver
import Data.Map as M
import Data.List
import Debug.Trace

getContext :: PropertiesObject obj => RefTable obj -> obj -> ObjectDefinition -> Name -> Context obj
getContext refTable i definitions name = Context {
        refTable = refTable,
        objects = objects,
        definition = definition,
        value = Nothing,
        R.name = name
    }
    where
        objects = fromList [(cINSTANCE, i)]
        definition = find isDef (properties definitions)
        isDef d = (D.name d) == name

getContextWithInstance :: PropertiesObject obj => Context obj -> obj -> Context obj
getContextWithInstance oldContext i = Context {
        refTable = refTable oldContext,
        objects = M.insert cINSTANCE i (objects oldContext),
        definition = definition oldContext,
        value = value oldContext,
        R.name = R.name oldContext
    }

getContextWithValue :: PropertiesObject obj => Context obj -> Value obj -> Context obj
getContextWithValue oldContext v = Context {
        refTable = refTable oldContext,
        objects = objects oldContext,
        definition = definition oldContext,
        value = Just v,
        R.name = R.name oldContext
    }

--- Getters ---

-- get object from context
getContextObj :: PropertiesObject obj => Context obj -> Name -> Maybe obj
getContextObj context name = M.lookup name (objects context)

-- get function from reference
getFunction :: RefTable obj -> Name -> Function obj
getFunction refTable name =
    case (M.lookup name refTable) of
        Just (RefFunc f) -> f
        _ -> error $ "unknown function " ++ name

-- get default expression from definition using context
getDefaultExpr :: PropertiesObject obj => Context obj -> Maybe Expr
getDefaultExpr context =
    case (definition context) of
        Just def -> Just (_default def)
        Nothing -> Nothing

-- get apply expression from definition using context
getApplyExpr :: PropertiesObject obj => Context obj -> Maybe Expr
getApplyExpr context =
    case (definition context) of
        Just def -> Just (apply def)
        Nothing -> Nothing

-- get apply expression from definition using context
getReadonlyExpr :: PropertiesObject obj => Context obj -> Maybe Expr
getReadonlyExpr context =
    case (definition context) of
        Just def -> Just (readonly def)
        Nothing -> Nothing

-- get valid expression from definition using context
getValidExpr :: PropertiesObject obj => Context obj -> Maybe Expr
getValidExpr context =
    case (definition context) of
        Just def -> Just (D.valid def)
        Nothing -> Nothing

-- get instance from context
getInstance :: PropertiesObject obj => Context obj -> obj
getInstance context =
    case (getContextObj context cINSTANCE) of
        Just i -> i
        _ -> error "it should have the instance"

-- get all definition property names
getDefinitionNames :: PropertiesObject obj => Context obj -> [Name]
getDefinitionNames context =
    let obj = getInstance context
    in case (get M.empty obj cMETA_DEFINITIONS) of
        Just (Object definitions) -> PO.all M.empty definitions
        _ -> []

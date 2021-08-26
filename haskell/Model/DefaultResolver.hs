module Model.DefaultResolver where

import Model.PropertiesObject
import Model.Resolver
import Model.Const
import Model.Context
import Model.Value
import Data.Map as M

defaultResolver :: PropertiesObject obj => Resolver obj
defaultResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt ->
        if hasIt then
            GNotResolved
        else case (getDefinitionValue context cDEFAULT) of
            Just v -> GResolved True
            _ -> GNotResolved,

    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value ->
        case value of
            Just v -> GNotResolved
            _ -> case (getDefinitionValue context cDEFAULT) of
                Just (Obj obj) -> case (getType obj) of
                    "case" -> case (evalCase context obj) of
                        Just w -> GResolved w
                        _ -> GResolved (Data "")
                    _ -> GResolved (Obj obj)
                Just v -> GResolved v
                _ -> GNotResolved,

    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}

evalCase :: PropertiesObject obj => Context obj -> obj -> Maybe (Value obj)
evalCase context caseObj =
    case (get M.empty caseObj "rules") of
        Just (List l) -> case (Prelude.foldr checkIf Nothing l) of
            Just v -> Just v
            Nothing -> doElse
        _ -> doElse
    where
        doElse = case (get M.empty caseObj "else") of
            Just w -> Just w
            _ -> Nothing
        checkIf (Obj ifObj) res = case (res) of
            Just v -> Just v
            Nothing -> case (get M.empty ifObj "if") of
                Just (Obj w) -> case (evalExpr context w) of
                    True -> get M.empty ifObj "value"
                    False -> Nothing
                Nothing -> Nothing

evalExpr :: PropertiesObject obj => Context obj -> obj -> Bool
evalExpr context exprObj = case (getType exprObj) of
    "equal" -> case (get M.empty exprObj "first") of
        Just (Obj first) -> case (get M.empty exprObj "second") of
            Just (Obj second) -> (evalValue context first) == (evalValue context second)
            _ -> False
        _ -> False
    _ -> error "unkown expression type"

evalValue :: PropertiesObject obj => Context obj -> obj -> String
evalValue context valueObj = case (getType valueObj) of
    "propvalue" -> case (get M.empty valueObj "name") of
        Just (Data name) -> case (get (refTable context) (getInstance context) name) of
            Just (Data v) -> v
            _ -> error $ "no value for property " ++ name
    "const" -> case (get M.empty valueObj "value") of
        Just (Data v) -> v
        _ -> error "invalid value"
    _ -> error "unknown value expression"

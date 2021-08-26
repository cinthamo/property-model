module Model.DefaultResolver where

import Model.PropertiesObject
import Model.Resolver
import Model.Const
import Model.Context
import Model.Value
import Data.Map as M

defaultResolver :: (Show obj, PropertiesObject obj) => Resolver obj
defaultResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt ->
        if hasIt then
            GNotResolved
        else case (getDefinitionValue context cDEFAULT) of
            Just (Object obj) -> case (getType obj) of
                "case" -> case (evalCase context obj) of
                    Just _ -> GResolved True
                    _ -> GNotResolved
                _ -> GResolved True
            Just v -> GResolved True
            _ -> GNotResolved,

    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value ->
        case value of
            Just v -> GNotResolved
            _ -> case (getDefinitionValue context cDEFAULT) of
                Just (Object obj) -> case (getType obj) of
                    "case" -> case (evalCase context obj) of
                        Just w -> GResolved w
                        _ -> GNotResolved
                    _ -> GResolved (Object obj)
                Just v -> GResolved v
                _ -> GNotResolved,

    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}

evalCase :: (Show obj, PropertiesObject obj) => Context obj -> obj -> Maybe (Value obj)
evalCase context caseObj =
    case (get M.empty caseObj "rules") of
        Just (List l) -> case (Prelude.foldr checkIf Nothing l) of
            Just v -> Just v
            Nothing -> doElse
    where
        doElse = case (get M.empty caseObj "else") of
            Just w -> Just (evalExpr context w)
            _ -> Nothing
        checkIf (Object ifObj) res = case (res) of
            Just v -> Just v
            Nothing -> case (get M.empty ifObj "if") of
                Just w -> case (evalExpr context w) of
                    Bool True -> get M.empty ifObj "value"
                    _ -> Nothing

evalExpr :: PropertiesObject obj => Context obj -> Value obj -> Value obj
evalExpr context exprObj = case (exprObj) of
    String s -> String s
    Number n -> Number n
    Bool b -> Bool b
    Reference name -> case (get (refTable context) (getInstance context) name) of
        Just v -> v
    Object fObj -> case (get M.empty fObj "f") of
        Just (Reference f) -> case (get M.empty fObj "p") of
            Just (List p) ->
                let func = getFunction (refTable context) f
                    parms = Prelude.map (evalExpr context) p
                in func parms

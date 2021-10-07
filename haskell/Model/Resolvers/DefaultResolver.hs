module Model.Resolvers.DefaultResolver where

import Model.PropertiesObject
import Model.Const
import Model.Context
import Model.Definition as D
import Model.Value as V
import Model.Resolvers.Resolver
import Data.Map as M

defaultResolver :: PropertiesObject obj => Resolver obj
defaultResolver = Resolver {
    beforeHas   = \context       -> GNotResolved,
    afterHas    = \context hasIt ->
        if hasIt then
            GNotResolved
        else case (getDefaultExpr context) of
            Just _ -> GResolved True
            _ -> GNotResolved,

    beforeGet   = \context       -> GNotResolved,
    afterGet    = \context value ->
        case value of
            Just v -> GNotResolved
            _ -> case (getDefaultExpr context) of
                Just expr -> GResolved (evalExpr context expr)
                _ -> GNotResolved,

    beforeSet   = \context value -> BSNotResolved,
    afterSet    = \context value -> ASNotResolved,
    beforeClear = \context       -> BSNotResolved,
    afterClear  = \context       -> ASNotResolved
}

evalExpr :: PropertiesObject obj => Context obj -> Expr -> Value obj
evalExpr context expr = case (expr) of
    Value v -> convertValue v
    Ref _ name ->
            case (get (refTable context) (getInstance context) name) of
                Just v -> v
    Case l e -> case (Prelude.foldr checkIf Nothing l) of
            Just v -> v
            Nothing -> case e of
                Just o -> evalExpr context o
                Nothing -> Null
        where
            checkIf (condition, value) res =
                case (res) of
                    Just x -> Just x
                    Nothing ->
                        case (evalExpr context condition) of
                            V.Bool True -> Just (evalExpr context value)
                            _ -> Nothing
    Call name p ->
        let func = getFunction (refTable context) name
            parms = Prelude.map (evalExpr context) p
        in func parms

convertValue :: PropertiesObject obj => ExprValue -> Value obj
convertValue (V (String s)) = String s
convertValue (V (Number n)) = Number n
convertValue (V (Bool b)) = Bool b
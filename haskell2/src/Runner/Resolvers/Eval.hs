module Runner.Resolvers.Eval where

import Model.Definition as D
import Model.Value as V
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Resolver

evalExpr :: PropertiesObject obj => Context obj -> Expr -> Value obj
evalExpr context expr = case expr of
  Value v -> convertValue v
  Ref _ name ->
    case get (refTable context) (getInstance context) name of
      Just v -> v
      Nothing -> error "Reference not found"
  RefValue ->
    case value context of
      Just v -> v
      Nothing -> error "Reference not found"
  ObjRef name ->
    case getContextObj context name of
      Just o -> Object o
      Nothing -> error "Reference not found"
  Case l e -> case Prelude.foldr checkIf Nothing l of
    Just v -> v
    Nothing -> case e of
      Just o -> evalExpr context o
      Nothing -> Null
    where
      checkIf (condition, value) res =
        case res of
          Just x -> Just x
          Nothing ->
            case evalExpr context condition of
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
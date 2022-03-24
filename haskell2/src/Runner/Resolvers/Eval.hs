module Runner.Resolvers.Eval where

import Model.Definition as D
import Model.Value as V
import Runner.Context
import Runner.PropertiesObject
import Runner.Resolvers.Resolver
import Data.String.Interpolate ( i )

evalExpr :: PropertiesObject obj => Show obj => Context obj -> Expr -> Value obj
evalExpr context expr = case expr of
  Value v -> convertValue v
  PropRef _ name ->
    case get (refTable context) (getInstance context) name of
      Just v -> v
      Nothing -> error [i|"Reference property #{name} not found"|]
  ValueRef ->
    case value context of
      Just v -> v
      Nothing -> error "Reference value not found"
  NameRef name ->
    case getByName context name of
      Just v -> v
      Nothing -> error [i|"Reference name #{name} not found"|]
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
convertValue (V Null) = Null
convertValue e = error [i|"Can't convert to value #{e}"|]

getByName :: PropertiesObject obj => Context obj -> Name -> Maybe (Value obj)
getByName context name =
  case getContextObj context name of
    Just v -> Just (Object v)
    Nothing -> get (refTable context) (getInstance context) name
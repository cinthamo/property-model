module Checker.Types where

import Data.List
import Data.String.Interpolate ( i )
import Model.Value
import Model.Definition as D
import Checker.TypeTable

typeCheck :: TypeTable -> DefinitionList -> Bool
typeCheck typeTable definition = Prelude.all f (properties definition)
  where
    f d =
      let pd = properties definition
       in validExpr pd typeTable (_default d) (_type d) (D.name d)
            && validExpr pd typeTable (apply d) TBool (D.name d)
            && validExpr pd typeTable (readonly d) TBool (D.name d)
            && validExpr pd typeTable (valid d) TBool (D.name d)

validExpr :: [Definition] -> TypeTable -> Expr -> ValueType -> Name -> Bool
validExpr pd r expr t n =
  let found = getType pd r expr t n
   in (found == t) || error ("Type mismatch in " ++ n ++ " found " ++ show found ++ " expected " ++ show t)

getType :: [Definition] -> TypeTable -> Expr -> ValueType -> Name -> ValueType
getType _ _ (Value (V (String _))) _ _ = TString
getType _ _ (Value (V (Number _))) _ _ = TNumber
getType _ _ (Value (V (Bool _))) _ _ = TBool
getType _ _ (Value (V _)) _ _ = TString
getType pd _ (PropRef (NameRef "this") prop) _ _ = maybe TString _type (find f pd) where f d = D.name d == prop
getType pd tt (PropRef expr prop) t n = getForProp tt (getType pd tt expr t n) prop
getType _ _ ValueRef t _ = t
getType _ tt (NameRef name) _ _ = getForObj tt name
getType pd r (Case conditions _otherwise) t n =
  if Prelude.all f conditions && g _otherwise then t else error "?"
  where
    f (c, v) = validExpr pd r c TBool n && validExpr pd r v t n
    g (Just v) = validExpr pd r v t n
    g Nothing = True
getType pd tt (Call name parameters) _ n =
  let types = getFuncParam tt name
      list = zip parameters types
      ok1 =
        (length types == length parameters + 1) || error [i|"Incorrect parameter lenght in #{n}, calling #{name} with #{length parameters} parameters, declared type #{length types} parameters"|]
      ok2 = Prelude.all f list
        where
          f (e, t) = validExpr pd tt e t n
   in if ok1 && ok2 then last types else error "?"

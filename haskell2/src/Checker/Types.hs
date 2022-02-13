module Checker.Types where

import Data.List
import Model.Const
import Model.Context (getFunctionTypes)
import Model.Definition as D
import Model.PropertiesObject
import Model.Resolvers.Resolver
import Model.Value

typeCheck :: PropertiesObject obj => RefTable obj -> DefinitionList -> Bool
typeCheck refTable definition = Prelude.all f (properties definition)
  where
    f d =
      let pd = properties definition
       in validExpr pd refTable (_default d) (_type d) (D.name d)
            && validExpr pd refTable (apply d) TBool (D.name d)
            && validExpr pd refTable (readonly d) TBool (D.name d)
            && validExpr pd refTable (valid d) TBool (D.name d)

validExpr :: [Definition] -> RefTable obj -> Expr -> ValueType -> Name -> Bool
validExpr pd r expr t n =
  let found = getType pd r expr t n
   in (found == t) || error ("Type mismatch in " ++ n ++ " found " ++ show found ++ " expected " ++ show t)

getType :: [Definition] -> RefTable obj -> Expr -> ValueType -> Name -> ValueType
getType _ _ (Value (V (String _))) _ _ = TString
getType _ _ (Value (V (Number _))) _ _ = TNumber
getType _ _ (Value (V (Bool _))) _ _ = TBool
getType _ _ (Value (V _)) _ _ = TUnknown
getType pd _ (Ref "this" prop) _ _ = maybe TUnknown _type (find f pd) where f d = D.name d == prop
getType _ _ (Ref _ prop) _ _ = TUnknown
getType _ _ RefValue t _ = t
getType _ _ (ObjRef _) _ _ = TUnknown
getType pd r (Case conditions _otherwise) t n =
  if Prelude.all f conditions && g _otherwise then t else error "?"
  where
    f (c, v) = validExpr pd r c TBool n && validExpr pd r v t n
    g (Just v) = validExpr pd r v t n
    g Nothing = True
getType pd refTable (Call name parameters) _ n =
  let types = getFunctionTypes refTable name
      list = zip parameters types
      ok1 =
        (length types == length parameters + 1) || error ("Incorrect parameter lenght in " ++ n)
      ok2 = Prelude.all f list
        where
          f (e, t) = validExpr pd refTable e t n
   in if ok1 && ok2 then last types else error "?"

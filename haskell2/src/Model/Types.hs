module Model.Types where

import Data.List
import Model.Const
import Model.Context (getFunctionTypes)
import Model.Definition as D
import Model.PropertiesObject
import Model.Resolvers.Resolver
import Model.Value

typeCheck :: PropertiesObject obj => RefTable obj -> ObjectDefinition -> Bool
typeCheck refTable definition = Prelude.all f (properties definition)
  where
    f d =
      let pd = properties definition
       in validExpr pd refTable (_default d) (_type d) (D.name d)
            && validExpr pd refTable (apply d) "Boolean" (D.name d)
            && validExpr pd refTable (readonly d) "Boolean" (D.name d)
            && validExpr pd refTable (valid d) "Boolean" (D.name d)

validExpr :: [Definition] -> RefTable obj -> Expr -> Name -> Name -> Bool
validExpr pd r expr t n =
  let found = getType pd r expr t n
   in (found == t) || error ("Type mismatch in " ++ n ++ " found " ++ found ++ " expected " ++ t)

getType :: [Definition] -> RefTable obj -> Expr -> Name -> Name -> Name
getType _ _ (Value (V (String _))) _ _ = "String"
getType _ _ (Value (V (Number _))) _ _ = "Number"
getType _ _ (Value (V (Bool _))) _ _ = "Boolean"
getType _ _ (Value (V _)) _ _ = "?"
getType pd _ (Ref "this" prop) _ _ = maybe "?" _type (find f pd) where f d = D.name d == prop
getType _ _ (Ref _ prop) _ _ = "?"
getType _ _ RefValue t _ = t
getType _ _ (ObjRef _) _ _ = "Object"
getType pd r (Case conditions _otherwise) t n =
  if Prelude.all f conditions && g _otherwise then t else error "?"
  where
    f (c, v) = validExpr pd r c "Boolean" n && validExpr pd r v t n
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

module Checker.Types where

import Data.List
import Data.String.Interpolate ( i )
import Model.Value
import Model.Definition as D
import Checker.TypeContext

typeCheck :: TypeContext -> DefinitionList -> Bool
typeCheck tc definition = Prelude.all f (properties definition)
  where
    f d = validExpr tc (_default d) (_type d) (D.name d)
       && validExpr tc (apply d)    TBool (D.name d)
       && validExpr tc (readonly d) TBool (D.name d)
       && validExpr tc (valid d)    TBool (D.name d)

validExpr :: TypeContext -> Expr -> ValueType -> Name -> Bool
validExpr tc expr vt n =
  let found = getType tc expr vt n
   in (found == vt) || error [i|"Type mismatch in #{n} found #{found} expected #{vt} in expression #{expr}"|]

getType :: TypeContext -> Expr -> ValueType -> Name -> ValueType
getType _ (Value (V (String _))) _ _ = TString
getType _ (Value (V (Number _))) _ _ = TNumber
getType _ (Value (V (Bool _))) _ _ = TBool
getType _ (Value (V t)) _ _ = error [i|"Unknown type #{t}"|]
getType _ ValueRef vt _ = vt
getType tc (PropRef expr prop) vt n = typeOfName (contextFor tc $ getType tc expr vt n) prop
getType tc (NameRef name) _ _ = typeOfName tc name
getType tc (Case conditions _otherwise) vt n =
  if Prelude.all f conditions && g _otherwise then vt else error "?"
  where
    f (c, v) = validExpr tc c TBool n && validExpr tc v vt n
    g (Just v) = validExpr tc v vt n
    g Nothing = True
getType tc (Call name parameters) _ n =
  let types = typesOfFuncParam tc name
      list = zip parameters types
      ok1 =
        (length types == length parameters + 1) || error [i|"Incorrect parameter lenght in #{n}, calling #{name} with #{length parameters} parameters, declared type #{length types} parameters"|]
      ok2 = Prelude.all f list
        where
          f (e, t) = validExpr tc e t n
   in if ok1 && ok2 then last types else error "?"

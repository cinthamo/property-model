module Checker.Types where

import Data.List
import Data.String.Interpolate ( i )
import Model.Value
import Model.Definition as D
import Checker.TypeContext

typeCheck :: TypeContext -> DefinitionList -> Bool
typeCheck tc definition = Prelude.all f (properties definition)
  where
    f d = validExpr tc (_default d) (_type d) (D.name d) True
       && validExpr tc (apply d)    TBool     (D.name d) False
       && validExpr tc (readonly d) TBool     (D.name d) False
       && validExpr tc (valid d)    TBool     (D.name d) False

validExpr :: TypeContext -> Expr -> ValueType -> Name -> Bool -> Bool
validExpr tc expr vt n optional =
  (optional && isNull expr)
  || (found == vt)
  || error [i|"Type mismatch in #{n} found #{found} expected #{vt} in expression #{expr}"|]
  where
    found = getType tc expr vt n
    isNull (Value (V Null)) = True
    isNull _ = False

getType :: TypeContext -> Expr -> ValueType -> Name -> ValueType
getType _ (Value (V (String _))) _ _ = TString
getType _ (Value (V (Number _))) _ _ = TNumber
getType _ (Value (V (Bool _))) _ _ = TBool
getType _ (Value (V t)) vt n = error [i|"Unknown type #{t} expected #{vt} in property #{n}"|]
getType _ ValueRef vt _ = vt
getType tc (PropRef expr prop) vt n = typeOfName (contextFor tc $ getType tc expr vt n) prop
getType tc (NameRef name) _ _ = typeOfName tc name
getType tc (Case conditions _otherwise) vt n =
  if Prelude.all f conditions && g _otherwise then vt else error "?"
  where
    f (c, v) = validExpr tc c TBool n False && validExpr tc v vt n False
    g (Just v) = validExpr tc v vt n False
    g Nothing = True
getType tc (Call name parameters) _ n =
  let types = typesOfFuncParam tc name
      list = zip parameters types
      ok1 =
        (length types == length parameters + 1) || error [i|"Incorrect parameter length in #{n}, calling #{name} with #{length parameters} parameters, declared type #{length types - 1} parameters"|]
      ok2 = Prelude.all f $ processGeneric list
        where
          f (e, t) = validExpr tc e t n False
          processGeneric [] = []
          processGeneric ((e, TGeneric m):l) = let t = getType tc e (TGeneric 0) n in (e, t):(processGeneric (map (replaceGeneric m t) l))
          processGeneric ((e, t): l) = (e, t):(processGeneric l)
          replaceGeneric x t (e, TGeneric z) = if (x == z) then (e, t) else (e, TGeneric z)                                            
          replaceGeneric _ _ (e, t) = (e, t)
   in if ok1 && ok2 then last types else error "?"

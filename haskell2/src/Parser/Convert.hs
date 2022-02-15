module Parser.Convert where

import Parser.PGrammar as G
import Model.Definition as D
import Model.Value as V
import Data.List

convert :: PDefinitionList -> DefinitionList
convert (PDefinitionList n t l) = DefinitionList {
    lname = n,
    externalType = t,
    properties = map convertDefinition l
}

convertDefinition :: PDefinition -> Definition
convertDefinition (PDefinition n l) = Definition {
    name = n,
    _type = convertType l,
    _default = convertRule l "default" D.null D.null D.null,
    apply = convertRule l "apply" true true false,
    readonly = convertRule l "readonly" false true false,
    valid = convertRule l "valid" true true false
}

convertType :: [PRule] -> ValueType
convertType l = case find f l of
        Just (ValueRule _ [] (Name t)) -> g t
        _ -> error "type not found"
    where
        f (ValueRule "type" _ _) = True
        f _ = False
        g t = case (lookup t types) of
                Just x -> x
                Nothing -> TExternal t

types :: [(String, ValueType)]
types = [
        ("string", TString),
        ("number", TNumber),
        ("boolean", TBool)
    ]

convertRule :: [PRule] -> String -> Expr -> Expr -> Expr -> Expr
convertRule l n defaultAbsent defaultUsed defaultOtherwise =
    case filter f l of
        [] -> defaultAbsent
        [x] -> g x
        _ -> error $ "multiple " ++ n ++ " not allowed"
    where
        f (ValueRule m _ _) = m == n
        f (SimpleRule m _) = m == n
        g (SimpleRule _ Nothing) = defaultUsed
        g (SimpleRule _ (Just x)) = Case [(convertExpr x, defaultUsed)] (Just defaultOtherwise)
        g (ValueRule _ [] v) = convertExpr v
        g (ValueRule _ l v) = Case (map h l) (Just $ convertExpr v)
        h (IfRule v x) = (convertExpr x, convertExpr v)

convertExpr :: PExpr -> Expr
convertExpr (G.Number n) = num n
convertExpr (G.Bool "false") = false
convertExpr (G.Bool "true") = true
convertExpr (G.String s) = str $ tail $ init s
convertExpr (G.Null _) = D.null
convertExpr (G.Value _) = ValueRef
convertExpr (G.Field e p) = PropRef (convertExpr e) p
convertExpr (G.Name n) = NameRef n
convertExpr (G.FuncCall n e1 el) = D.Call n $ map convertExpr (e1:el)
convertExpr (G.MethCall et n e1 el) = D.Call n $ map convertExpr (et:(e1:el))
convertExpr (G.OpCall e1 n e2) = D.Call n [convertExpr e1, convertExpr e2]
convertExpr (G.Not e) = D.Call "not" [convertExpr e]

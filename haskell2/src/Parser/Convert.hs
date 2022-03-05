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
convertDefinition (PDefinition d n t l) = Definition {
    name = n,
    _type = convertType t,
    doc = d,
    _default = convertRule l "default" D.null D.null D.null,
    apply = convertRule l "apply" true true false,
    readonly = convertRule l "readonly" false true false,
    valid = convertRule l "valid" true true false
}

convertType :: String -> ValueType
convertType t = case (lookup t types) of
                    Just x -> x
                    Nothing -> TExternal t

types :: [(String, ValueType)]
types = [
        ("string", TString),
        ("numeric", TNumber),
        ("boolean", TBool)
    ]

convertRule :: Maybe [PRule] -> String -> Expr -> Expr -> Expr -> Expr
convertRule rules n defaultAbsent defaultUsed defaultOtherwise =
    case rules of
        Nothing -> defaultAbsent
        Just l ->
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
convertExpr (G.Call n el) = D.Call n $ map convertExpr el

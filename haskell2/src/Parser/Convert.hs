module Parser.Convert where

import Parser.PGrammar as G
import Model.Definition as D
import Model.Value as V
import Data.List

convert :: [PDefinition] -> ObjectDefinition
convert l = ObjectDefinition {
    properties = map convertDefinition l,
    related = []
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

convertType :: [Rule] -> String
convertType l = case find f l of
        Just (Rule _ (Just (G.String t)) Nothing) -> tail $ init t
        _ -> error "type not found"
    where
        f (Rule "type" _ _) = True
        f _ = False

convertRule :: [Rule] -> String -> Expr -> Expr -> Expr -> Expr
convertRule l n defaultAbsent defaultUsed defaultOtherwise =
    case filter f l of
        [] -> defaultAbsent
        [Rule _ Nothing Nothing] -> defaultUsed
        [Rule _ (Just e) Nothing] -> convertExpr e
        l -> g l
    where
        f (Rule m _ _) = m == n
        g [] = defaultOtherwise
        g (Rule _ r Nothing:_) = h r
        g (Rule _ r (Just x):l) =
            let
                y = (convertExpr x, h r)
            in case g l of
                Case m e -> Case (y:m) e
                e -> Case [y] (Just e)
        h Nothing = defaultUsed
        h (Just e) = convertExpr e

convertExpr :: PExpr -> Expr
convertExpr (G.Number n) = num n
convertExpr (G.Bool "false") = false
convertExpr (G.Bool "true") = true
convertExpr (G.String s) = str $ tail $ init s
convertExpr (G.Null _) = D.null
convertExpr (G.Value _) = RefValue
convertExpr (Field o p) = Ref o p
convertExpr (ThisField p) = Ref "this" p
convertExpr (G.Call n e1 el) = D.Call n $ map convertExpr (e1:el)
convertExpr (OpCall e1 n e2) = D.Call n [convertExpr e1, convertExpr e2]
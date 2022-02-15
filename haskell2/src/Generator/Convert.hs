module Generator.Convert (convert) where

import Data.List
import Model.Definition
import Model.Value
import Generator.Data as D

convert :: DefinitionList -> GDefinitionList
convert (DefinitionList _ _ props) = GDefinitionList {
        D.properties = convertProperties props,
        D.defaultResolvers = convertDefaultResolvers props,
        D.applyResolvers = convertApplyResolvers props,
        D.readonlyResolvers = convertReadonlyResolvers props,
        D.validResolvers = convertValidResolvers props
    }

convertProperties :: [Definition] -> [GDefinition]
convertProperties props = map convertOne $ filter isDefinition props
    where
        isDefinition (Definition _ _ _ _ _ _) = True
        isDefinition _ = False

convertOne :: Definition -> GDefinition
convertOne (Definition name _type _default apply readonly valid) = GDefinition {
        D.id = name,
        D.name = name,
        D.aType = convertTypeGxp _type,
        D.customType = convertCustomType _type,
        D.aDefault = convertDefault _default
    }

convertTypeGxp :: ValueType -> String
convertTypeGxp TString = "Text"
convertTypeGxp TNumber = "Integer"
convertTypeGxp TBool = "Boolean"
convertTypeGxp (TExternal s) = "Custom"

convertCustomType :: ValueType -> Maybe String
convertCustomType (TExternal s) = Just s
convertCustomType _ = Nothing

convertDefault :: Expr -> Maybe String
convertDefault (Value (V (String s))) = Just s
convertDefault (Value (V (Number n))) = Just (show n)
convertDefault (Value (V (Bool True))) = Just "true"
convertDefault (Value (V (Bool False))) = Just "false"
convertDefault _ = Nothing

convertDefaultResolvers :: [Definition] -> [GResolver]
convertDefaultResolvers props = map convertDefaultResolverOne $ filter needResolver props
    where
        needResolver (Definition _ _ _default _ _ _) = convertDefault _default == Nothing
        needResolver _ = False
        convertDefaultResolverOne (Definition name _ _default _ _ _) = convertResolverOne name _default "Default" [] [stReturn (convertExpr true)] (stAssign False "value")

convertApplyResolvers :: [Definition] -> [GResolver]
convertApplyResolvers props = map convertApplyResolverOne $ filter needResolver props
    where
        needResolver (Definition _ _ _ apply _ _) = apply /= true
        needResolver _ = False
        convertApplyResolverOne (Definition name _ _ apply _ _) = convertResolverOne name apply "Apply" [] [] stReturn

convertReadonlyResolvers :: [Definition] -> [GResolver]
convertReadonlyResolvers props = map convertReadonlyResolverOne $ filter needResolver props
    where
        needResolver (Definition _ _ _ _ readonly _) = readonly /= false
        needResolver _ = False
        convertReadonlyResolverOne (Definition name _ _ _ readonly _) = convertResolverOne name readonly "Readonly" [] [] stReturn

convertValidResolvers :: [Definition] -> [GResolver]
convertValidResolvers props = map convertValidResolverOne $ filter needResolver props
    where
        needResolver (Definition _ _ _ _ _ valid) = valid /= true
        needResolver _ = False
        convertValidResolverOne (Definition name _type _ _ _ valid) = convertResolverOne name valid "Valid" [stAssign True "typedValue" (exprCast (convertTypeDotNet _type) (exprConstant "value"))] [] stReturn

convertResolverOne :: String -> Expr -> String -> [GStatement] -> [GStatement] -> (GExpr -> GStatement) -> GResolver
convertResolverOne name code suffix before after end = GResolver {
        D.propName = name,
        D.className = name ++ suffix,
        D.code = before ++ convertCode end code ++ after,
        D.used = nub $ usedProperties code
    }

convertCode :: (GExpr -> GStatement) -> Expr -> [GStatement]
convertCode end (Case [(condition,result)] otherwise) = [
        stIf (convertExpr condition) (convertCode end result) (fmap (convertCode end) otherwise)
    ]
convertCode end (Case ((condition,result):more) otherwise) = [
        stIf (convertExpr condition) (convertCode end result) (Just (convertCode end (Case more otherwise)))
    ]
convertCode end e = [end (convertExpr e)]

usedProperties :: Expr -> [String]
usedProperties (PropRef expr prop) = (usedProperties expr) ++ [prop]
usedProperties (Case conditions otherwise) = (concat $ map (usedProperties . fst) conditions) ++ (concat $ map (usedProperties . snd) conditions) ++ f otherwise
    where
        f Nothing = []
        f (Just x) = usedProperties x
usedProperties (Call _ param) = concat $ map usedProperties param
usedProperties _ = []

convertExpr :: Expr -> GExpr
convertExpr (Value (V v)) = exprConstant $ convertValue v
convertExpr (NameRef prop) = exprGetProp $ GGetProp (convertTypeDotNet TNumber) prop
convertExpr ValueRef = exprConstant "typedValue"
convertExpr (Call name lexpr) =
    case (elem name operators) of
        False -> exprCall $ GCall name (map convertExpr lexpr)
        True -> exprOperator $ GOperator name (convertExpr $ head lexpr) (convertExpr $ head $ tail lexpr)
convertExpr e = error $ "Not yet supported expression " ++ (show e)

convertValue :: Value obj -> String
convertValue (String s) = "\"" ++ s ++ "\""
convertValue (Number n) = show n
convertValue (Bool True) = "true"
convertValue (Bool False) = "false"
convertValue Null = "null"
convertValue (Object o) = error "No supported value Object"
convertValue (List x) = error "No supported value List"

convertTypeDotNet :: ValueType -> String
convertTypeDotNet _ = "int"

operators :: [String]
operators = ["==", "+", ">", "<"]

-- Statements

stAssign :: Bool -> String -> GExpr -> GStatement
stAssign d varName expr = GStatement {
        assign = Just (GAssign {
            D.declare = d,
            D.varName = varName,
            D.expr = expr
        }),
        aIf = Nothing,
        aReturn = Nothing
    }

stIf :: GExpr -> [GStatement] -> Maybe [GStatement] -> GStatement
stIf condition trueBlock falseBlock = GStatement {
            assign = Nothing,
            aIf = Just (GIf {
                D.condition = condition,
                D.trueBlock = trueBlock,
                D.falseBlock = falseBlock
            }),
            aReturn = Nothing
        }

stReturn :: GExpr -> GStatement
stReturn expr = GStatement {
        assign = Nothing,
        aIf = Nothing,
        aReturn = Just expr
    }

-- Expression

exprConstant :: String -> GExpr
exprConstant s = GExpr {
        constant = Just s,
        getProp = Nothing,
        call = Nothing,
        operator = Nothing,
        cast = Nothing
    }

exprGetProp :: GGetProp -> GExpr
exprGetProp gp = GExpr {
        constant = Nothing,
        getProp = Just gp,
        call = Nothing,
        operator = Nothing,
        cast = Nothing
    }

exprCall :: GCall -> GExpr
exprCall c = GExpr {
        constant = Nothing,
        getProp = Nothing,
        call = Just c,
        operator = Nothing,
        cast = Nothing
    }

exprOperator :: GOperator -> GExpr
exprOperator op = GExpr {
        constant = Nothing,
        getProp = Nothing,
        call = Nothing,
        operator = Just op,
        cast = Nothing
    }

exprCast :: String -> GExpr -> GExpr
exprCast t e = GExpr {
        constant = Nothing,
        getProp = Nothing,
        call = Nothing,
        operator = Nothing,
        cast = Just $ GCast t e
    }

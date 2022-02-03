module Generator.Convert (convert) where

import Data.List
import Model.Definition
import Model.Value
import Generator.Data as D

convert :: DefinitionList -> GDefinitionList
convert (DefinitionList _ props _) = GDefinitionList {
        D.properties = convertProperties props,
        D.defaultResolvers = convertDefaultResolvers props
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
        D.aDefault = convertDefault _default,
        D.defaultResolver = convertDefaultResolver _default,
        D.applyResolver = convertApplyResolver apply,
        D.readonlyResolver = convertReadonlyResolver readonly,
        D.validResolver = convertValidResolver valid
    }

convertTypeGxp :: ValueType -> String
convertTypeGxp TString = "Text"
convertTypeGxp TNumber = "Integer"
convertTypeGxp TBool = "Boolean"
convertTypeGxp TUnknown = error "Can't generate a property with unknown type"
convertTypeGxp (TCustom s) = "Custom"

convertCustomType :: ValueType -> Maybe String
convertCustomType (TCustom s) = Just s
convertCustomType _ = Nothing

convertDefault :: Expr -> Maybe String
convertDefault (Value (V (String s))) = Just s
convertDefault (Value (V (Number n))) = Just (show n)
convertDefault (Value (V (Bool True))) = Just "true"
convertDefault (Value (V (Bool False))) = Just "false"
convertDefault _ = Nothing

convertDefaultResolver :: Expr -> Maybe String
convertDefaultResolver e = Nothing

convertApplyResolver :: Expr -> Maybe String
convertApplyResolver e = Nothing

convertReadonlyResolver :: Expr -> Maybe String
convertReadonlyResolver e = Nothing

convertValidResolver :: Expr -> Maybe String
convertValidResolver e = Nothing

convertDefaultResolvers :: [Definition] -> [GDefaultResolver]
convertDefaultResolvers props = map convertDefaultResolversOne $ filter needResolver props
    where
        needResolver (Definition _ _ _default _ _ _) = convertDefault _default == Nothing
        needResolver _ = False

convertDefaultResolversOne :: Definition -> GDefaultResolver
convertDefaultResolversOne (Definition name _ _default _ _ _) = GDefaultResolver {
        D.propName = name,
        D.className = name ++ "DefaultResolver",
        D.code = convertCode _default,
        D.used = nub $ usedProperties _default
    }

convertCode :: Expr -> [GStatement]
convertCode (Case [(condition,result)] otherwise) = [
        GStatement {
            assign = Nothing,
            aIf = Just (GIf {
                D.condition = convertExpr condition,
                D.trueBlock = convertCode result,
                D.falseBlock = fmap convertCode otherwise
            })
        }
    ]
convertCode (Case ((condition,result):more) otherwise) = [
        GStatement {
            assign = Nothing,
            aIf = Just (GIf {
                D.condition = convertExpr condition,
                D.trueBlock = convertCode result,
                D.falseBlock = Just (convertCode (Case more otherwise))
            })
        }
    ]
convertCode e = [
        GStatement {
            assign = Just (GAssign {
                D.varName = "value",
                D.expr = convertExpr e
            }),
            aIf = Nothing
        }
    ]

usedProperties :: Expr -> [String]
usedProperties (Ref _ prop) = [prop]
usedProperties (Case conditions otherwise) = (concat $ map (usedProperties . fst) conditions) ++ (concat $ map (usedProperties . snd) conditions) ++ f otherwise
    where
        f Nothing = []
        f (Just x) = usedProperties x
usedProperties (Call _ param) = concat $ map usedProperties param
usedProperties _ = []

convertExpr :: Expr -> GExpr
convertExpr (Value (V v)) = GExpr {
        constant = Just $ convertValue v,
        getProp = Nothing,
        call = Nothing,
        operator = Nothing
    }
convertExpr (Ref "this" prop) = GExpr {
        constant = Nothing,
        getProp = Just $ GGetProp (convertTypeDotNet prop) prop,
        call = Nothing,
        operator = Nothing
    }
convertExpr (Call name lexpr) =
    case (elem name operators) of
        False -> GExpr {
            constant = Nothing,
            getProp = Nothing,
            call = Just $ GCall name (map convertExpr lexpr),
            operator = Nothing
        }
        True -> GExpr {
            constant = Nothing,
            getProp = Nothing,
            call = Nothing,
            operator = Just $ GOperator name (convertExpr $ head lexpr) (convertExpr $ head $ tail lexpr)
        }
convertExpr e = error $ "Not yet supported expression " ++ (show e)

convertValue :: Value obj -> String
convertValue (String s) = "\"" ++ s ++ "\""
convertValue (Number n) = show n

convertTypeDotNet :: String -> String
convertTypeDotNet _ = "int"

operators :: [String]
operators = ["==", "+"]

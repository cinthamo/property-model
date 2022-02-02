module Generator.Convert (convert) where

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
        D.aType = convertType _type,
        D.customType = convertCustomType _type,
        D.aDefault = convertDefault _default,
        D.defaultResolver = convertDefaultResolver _default,
        D.applyResolver = convertApplyResolver apply,
        D.readonlyResolver = convertReadonlyResolver readonly,
        D.validResolver = convertValidResolver valid
    }

convertType :: ValueType -> String
convertType TString = "Text"
convertType TNumber = "Integer"
convertType TBool = "Boolean"
convertType TUnknown = error "Can't generate a property with unknown type"
convertType (TCustom s) = "Custom"

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
convertDefaultResolversOne (Definition name _ _ _ _ _) = GDefaultResolver {
        D.propName = name,
        D.className = name ++ "DefaultResolver"
    }

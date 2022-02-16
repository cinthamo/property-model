module Checker.TypeContext where

import Data.Maybe
import Data.List
import Data.String.Interpolate ( i )
import Model.Value
import Model.Definition

data TypeContext =
    TypeContext {
        current :: TypeObject,
        list :: TExternals
    }

type TNames = [(Name,ValueType)]
type TFunctions = [(Name,[ValueType])]
type TExternals = [(ValueType,TypeObject)]
type TEnums = [(Name,[Name])]

data TypeObject =
    TypeObject {
        tnames :: TNames,
        tfunctions :: TFunctions
    }

typeOfName :: TypeContext -> Name -> ValueType
typeOfName context n = case (lookup n $ tnames $ current context) of
    Just x -> x
    Nothing -> error [i|"Name #{n} not found in context"|]

typesOfFuncParam :: TypeContext -> Name -> [ValueType]
typesOfFuncParam context n = case (lookup n $ tfunctions $ current context) of
    Just x -> x
    Nothing -> error [i|"Function #{n} not found in context"|]

contextFor :: TypeContext -> ValueType -> TypeContext
contextFor context vt = let l = list context
    in case (lookup vt l) of
        Just x -> TypeContext x l
        Nothing -> error [i|"Type #{vt} not found in context"|]

newTypeContext :: DefinitionList -> TFunctions -> TExternals -> TEnums -> TypeContext
newTypeContext definitionList functions externals enums = 
    TypeContext {
        current = TypeObject {
            tnames = tnames dto ++ map convertNameEnum enums,
            tfunctions = tfunctions dto ++ functions
        },
        list = externals ++ map convertListEnum enums ++ [convertDefinition definitionList]
    }
    where
        emptyTO = TypeObject [] []
        eto = maybe emptyTO (\n -> fromMaybe emptyTO $ lookup (TExternal n) externals) (externalType definitionList)
        dto = TypeObject {
                tnames = (map (\d -> (name d, _type d)) $ properties definitionList) ++ tnames eto,
                tfunctions = tfunctions eto
            }
        convertNameEnum (name,_) = (name, TEnum name)
        convertListEnum (name,values) = (TEnum name, TypeObject (map (\v -> (v, TEnumValue name)) values) [])
        convertDefinition definitionList = (TInternal $ lname definitionList, dto)

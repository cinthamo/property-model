module Generator.Data where

import Data.Data
import Data.Typeable

data GDefinitionList
  = GDefinitionList
      { properties :: [GDefinition],
        defaultResolvers :: [GDefaultResolver]
      }
  deriving (Show, Data, Typeable)

data GDefinition
  = GDefinition
      { id :: String,
        name :: String,
        aType :: String,
        customType :: Maybe String,
        aDefault :: Maybe String,
        defaultResolver :: Maybe String,
        applyResolver :: Maybe String,
        readonlyResolver :: Maybe String,
        validResolver :: Maybe String
      }
  deriving (Show, Data, Typeable)

data GDefaultResolver
  = GDefaultResolver
      {
        propName :: String,
        className :: String,
        code :: [GStatement],
        used :: [String]
      }
  deriving (Show, Data, Typeable)

data GStatement
  = GStatement
      {
        assign :: Maybe GAssign,
        aIf :: Maybe GIf
      }
  deriving (Show, Data, Typeable)

data GAssign
  = GAssign
      {
        varName :: String,
        expr :: GExpr
      }
  deriving (Show, Data, Typeable)
  
data GIf
  = GIf
      {
        condition :: GExpr,
        trueBlock :: [GStatement],
        falseBlock :: Maybe [GStatement]
      }
  deriving (Show, Data, Typeable)

data GExpr
  = GExpr {
    constant :: Maybe String,
    getProp :: Maybe GGetProp,
    call :: Maybe GCall,
    operator :: Maybe GOperator
  }
  deriving (Show, Data, Typeable)

data GGetProp
  = GGetProp {
    pType :: String,
    pName :: String
  }
  deriving (Show, Data, Typeable)

data GCall
  = GCall {
    fname :: String,
    param :: [GExpr]
  }
  deriving (Show, Data, Typeable)

data GOperator
  = GOperator {
    symbol :: String,
    left :: GExpr,
    right :: GExpr
  }
  deriving (Show, Data, Typeable)

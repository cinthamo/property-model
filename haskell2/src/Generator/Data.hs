module Generator.Data where

import Data.Data
import Data.Typeable

data GDefinitionList
  = GDefinitionList
      { properties :: [GDefinition],
        defaultResolvers :: [GResolver],
        applyResolvers :: [GResolver],
        readonlyResolvers :: [GResolver],
        validResolvers :: [GResolver]
      }
  deriving (Show, Data, Typeable)

data GDefinition
  = GDefinition
      { id :: String,
        name :: String,
        aType :: String,
        customType :: Maybe String,
        aDefault :: Maybe String
      }
  deriving (Show, Data, Typeable)

data GResolver
  = GResolver
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
        aIf :: Maybe GIf,
        aReturn :: Maybe GExpr
      }
  deriving (Show, Data, Typeable)

data GAssign
  = GAssign
      {
        declare :: Bool,
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
    getPropInt :: Maybe GGetPropInt,
    getPropExt :: Maybe GGetPropExt,
    call :: Maybe GCall,
    operator :: Maybe GOperator,
    cast :: Maybe GCast
  }
  deriving (Show, Data, Typeable)

data GGetPropInt
  = GGetPropInt {
    pType :: String,
    pName :: String
  }
  deriving (Show, Data, Typeable)

data GGetPropExt
  = GGetPropExt {
    target :: GExpr,
    qName :: String
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

data GCast
  = GCast {
    cType :: String,
    cExpr :: GExpr
  }
  deriving (Show, Data, Typeable)
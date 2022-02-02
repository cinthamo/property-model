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
        className :: String
      }
  deriving (Show, Data, Typeable)

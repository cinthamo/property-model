module External.BasicTable where

import Data.Map as M
import External.Functions
import Model.PropertiesMap
import Model.Resolvers.Resolver

basicRef :: RefTable PropertiesMap
basicRef =
  M.fromList
    [ ("==", RefFunc equal ["number", "number", "boolean"]),
      ("+", RefFunc add ["number", "number", "number"]),
      (">", RefFunc greater ["number", "number", "boolean"])
    ]
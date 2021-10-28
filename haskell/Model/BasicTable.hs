module Model.BasicTable where

import Data.Map as M
import Model.Functions
import Model.PropertiesMap
import Model.Resolvers.Resolver

basicRef :: RefTable PropertiesMap
basicRef =
  M.fromList
    [ ("==", RefFunc equal ["Number", "Number", "Boolean"]),
      ("+", RefFunc add ["Number", "Number", "Number"]),
      (">", RefFunc greater ["Number", "Number", "Boolean"])
    ]
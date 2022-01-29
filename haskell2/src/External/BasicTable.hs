module External.BasicTable where

import Data.Map as M
import External.Functions
import Model.PropertiesMap
import Model.Resolvers.Resolver
import Model.Value

basicRef :: RefTable PropertiesMap
basicRef =
  M.fromList
    [ ("==", RefFunc equal [TNumber, TNumber, TBool]),
      ("+", RefFunc add [TNumber, TNumber, TNumber]),
      (">", RefFunc greater [TNumber, TNumber, TBool])
    ]
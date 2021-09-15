module Model.BasicTable where

import Data.Map as M
import Model.PropertiesMap
import Model.Functions
import Model.Resolvers.Resolver

basicRef :: RefTable PropertiesMap
basicRef = M.fromList [
        ("equal", RefFunc equal),
        ("add", RefFunc add)
    ]
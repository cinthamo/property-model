module Model.BasicTable where

import Data.Map as M
import Model.PropertiesMap
import Model.Functions
import Model.Resolvers.Resolver
import Model.Resolvers.DefaultResolver

basicRef :: RefTable PropertiesMap
basicRef = M.fromList [
        ("Resolvers.Default", RefRes defaultResolver),
        ("equal", RefFunc equal),
        ("add", RefFunc add)
    ]
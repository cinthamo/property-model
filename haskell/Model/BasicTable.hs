module Model.BasicTable where

import Data.Map as M
import Model.Resolver
import Model.PropertiesMap
import Model.DefaultResolver

basicRef :: RefTable PropertiesMap
basicRef = M.fromList [("Resolvers.Default", RefRes defaultResolver)]
module Main where

import Test
import Model.Value
import Model.PropertiesMap
import Model.PropertiesObject
import Model.BasicTable
import Model.Resolvers.AspectResolver
import Func.Static

main :: IO ()
main = do
    print . show $ process1
    print . show $ process2
    print . show $ process3
    where
        obj = emptyWAsp definitions
        process1 = get basicRef obj "one"
        process2 = get basicRef obj "two"
        process3 = static basicRef obj

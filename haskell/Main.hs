module Main where

import Func.Static
import Model.BasicTable
import Model.PropertiesMap
import Model.PropertiesObject
import Model.Resolvers.AspectResolver
import Model.Value
import Test

main :: IO ()
main = do
  print . show $ process1
  print . show $ process2
  print . show $ process3
  print . show $ process4
  print . show $ process5
  where
    obj = emptyWAsp definitions
    process1 = get basicRef obj "one"
    process2 = get basicRef obj "two"
    process3 = static basicRef (set basicRef obj "one" (Number 1))
    process4 = get basicRef (set basicRef obj "four" (Number 1)) "four"
    process5 = get basicRef (set basicRef obj "four" (Number 4)) "four"

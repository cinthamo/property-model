module Test.Manual (test) where

import Checker.Types
import Model.Definition
import Model.Value as V
import External.BasicTable
import External.TypeTable
import Runner.PropertiesMap
import Runner.PropertiesObject
import Runner.Resolvers.AspectResolver
import Test.Static

test :: DefinitionList -> IO ()
test definitions = do
  print $ "typecheck " ++ show check
  print process1
  print process2
  print process3
  print process4
  print process5
  where
    obj = emptyWAsp $ definitions
    check = typeCheck typeTable $ definitions -- typeChecker test
    process1 = get basicRef obj "one" -- simple get
    process2 = get basicRef obj "two" -- default test
    process3 = static basicRef (set basicRef obj "one" (V.Number 1)) -- static, apply & readonly test
    process4 = get basicRef (set basicRef obj "four" (V.Number 1)) "four" -- valid test (invalid value)
    process5 = get basicRef (set basicRef obj "four" (V.Number 4)) "four" -- valid test (valid value)
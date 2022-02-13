module Lib (p,f) where

import Text.Pretty.Simple (pPrint)
import Data.List
import Data.Maybe
import Generator.Gen
import Test.Manual
import Model.Definition
import Parser.File

p :: IO ()
p = do
        x <- readT
        pPrint x
        --test x

f :: IO ()
f = do
        x <- readT
        check x
        gen x

readT :: IO DefinitionList
readT = do
        ast <- parseFile "test.gxp"
        let x = fromJust $ find (\x -> lname x == "Test") ast
        return x

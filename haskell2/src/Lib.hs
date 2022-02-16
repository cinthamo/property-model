module Lib (p,f) where

import Text.Pretty.Simple (pPrint)
import Data.List
import Data.Maybe
import Data.String.Interpolate ( i )
import Generator.Gen
import Test.Manual
import Model.Definition
import Parser.File
import Checker.Check

p :: String -> IO ()
p name = do
        x <- readT name
        pPrint x
        --test x

f :: String -> IO ()
f name = do
        x <- readT name
        check x
        gen x

readT :: String -> IO DefinitionList
readT name = do
        ast <- parseFile "test.gxp"
        let x = case (find (\x -> lname x == name) ast) of
                        Just y -> y
                        _ -> error [i|"Definitions for #{name} not found"|]
        return x

module Lib where

import Text.Pretty.Simple (pPrint)
import Data.List
import Data.Maybe
import Data.String.Interpolate ( i )
import Generator.Gen
import Test.Manual
import Model.Definition
import Parser.File
import Checker.Check
import Runner.Interactive as I

printOne :: String -> String -> IO ()
printOne name file = do
        x <- readT name file
        pPrint x
        --test x

checkAll :: String -> IO ()
checkAll file = do
        ast <- readL file
        pPrint $ map (\d -> lname d ++ (if (check d) then " OK" else " Fail")) ast

genOne :: String -> String -> IO ()
genOne name file = do
        x <- readT name file
        print $ check x
        gen x

interactive :: String -> IO()
interactive file = do
        x <- readL file
        I.interactive x

readL :: String -> IO [DefinitionList]
readL file = do
        ast <- parseFile file
        return ast

readT :: String -> String -> IO DefinitionList
readT name file = do
        ast <- parseFile file
        let x = case (find (\x -> lname x == name) ast) of
                        Just y -> y
                        _ -> error [i|"Definitions for #{name} not found"|]
        return x

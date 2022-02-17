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

printOne :: String -> IO ()
printOne name = do
        x <- readT name
        pPrint x
        --test x

checkAll :: IO ()
checkAll = do
        ast <- readL
        pPrint $ map (\d -> (lname d, check d)) ast

genOne :: String -> IO ()
genOne name = do
        x <- readT name
        print $ check x
        gen x

readL :: IO [DefinitionList]
readL = do
        ast <- parseFile "test.p"
        return ast

readT :: String -> IO DefinitionList
readT name = do
        ast <- parseFile "test.p"
        let x = case (find (\x -> lname x == name) ast) of
                        Just y -> y
                        _ -> error [i|"Definitions for #{name} not found"|]
        return x

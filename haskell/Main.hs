module Main where

import Model.Serializer
import Model.Value
import Model.PropertiesMap
import Model.PropertiesObject
import Model.BasicTable

main :: IO ()
main = do
    contents <- readFile "test.json" :: IO String
    print . show . process $ contents
    where
        value c = fromString c :: PropertiesMap
        process c = get basicRef (value c) "one"

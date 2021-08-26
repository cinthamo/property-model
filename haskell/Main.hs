module Main where

import Model.Serializer
import Model.Value
import Model.PropertiesMap
import Model.PropertiesObject
import Model.BasicTable

main :: IO ()
main = do
    contents <- readFile "test.json" :: IO String
    print . show . process1 $ contents
    print . show . process2 $ contents
    where
        value c = fromString c :: PropertiesMap
        process1 c = get basicRef (value c) "one"
        process2 c = get basicRef (value c) "two"

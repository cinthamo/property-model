module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["p"] -> p "Test" -- pretty print example
        [s] -> f s -- main function
        [] -> f "Test" -- default

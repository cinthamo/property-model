module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["p"] -> printOne "Test" -- pretty print example
        ["p",n] -> printOne n -- pretty print example
        [s] -> genOne s -- check and generate
        [] -> checkAll -- default

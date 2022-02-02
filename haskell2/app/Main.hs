module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> p -- pretty print example
        _ -> f -- main function

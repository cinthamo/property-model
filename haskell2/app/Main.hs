module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        [n] -> case (lookup n commandsFile) of
            Just c -> c "Test.p"
            Nothing -> case (lookup n commandsOne) of
                Just c -> c "Test" "test.p"
                Nothing -> error "Unknown command"
        [n,x] -> case (lookup n commandsFile) of
            Just c -> c x
            Nothing -> case (lookup n commandsOne) of
                Just c -> c x "test.p"
                Nothing -> error "Unknown command"
        [n,x,y] -> case (lookup n commandsOne) of
                Just c -> c x y
                Nothing -> error "Unknown command"
        _ -> error "Unknown command"

commandsFile :: [( String, String -> IO() )]
commandsFile = [
        ("a", checkAll),
        ("i", interactive)
    ]

commandsOne :: [(String, String -> String -> IO())]
commandsOne = [
        ("p", printOne),
        ("g", genOne)
    ]

usage :: IO ()
usage = do
    putStrLn "Usage"
    putStrLn "  a <File> - check all of <File>"
    putStrLn "  p <Typd> <File> - print <Type> of <File>"
    putStrLn "  g <Type> <File> - check and generate <Type> of <File>"
    putStrLn "  l <File> - list types in <File>"
    putStrLn "  i <File> - interactive prompt with <File>"
    putStrLn ""
    putStrLn "parameters are optional, if no specified"
    putStrLn "<Type> is Test and <File> is test.p"
    putStrLn ""
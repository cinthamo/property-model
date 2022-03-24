module Runner.Interactive (interactive) where

import Text.Pretty.Simple (pPrint)
import Data.List
import Data.List.Split
import Data.Maybe
import Model.Definition as D
import Model.Value
import Runner.Context
import Runner.PropertiesObject as PO
import Runner.PropertiesInfo
import Runner.Resolvers.Resolver
import Runner.Resolvers.AspectResolver
import External.BasicTable

interactive :: String -> [DefinitionList] -> IO ()
interactive file d = do
    loop basicRef def obj
    where
        def = head d
        obj = emptyWAsp def
        jsonFile = file ++ ".json"


loop :: PropertiesObject obj => PropertiesInfo obj => Show obj => RefTable obj -> DefinitionList -> obj -> IO ()
loop refTable def obj = do
    putStrLn "> "
    line <- getLine
    newObj <- exec refTable def obj $ splitOn " " line
    if (line == "x") then
        return ()
    else
        loop refTable def newObj

exec :: PropertiesObject obj => PropertiesInfo obj => Show obj => RefTable obj -> DefinitionList -> obj -> [String] -> IO obj
exec _ _ obj [""] = usage >> return obj
exec refTable _ obj ["p"] = printObj refTable obj >> return obj
exec refTable _ obj ["g",n] = print (get refTable obj n) >> return obj
exec refTable _ obj ["s",n,v] = return $ set refTable obj n (convertValue v)
exec refTable def obj ["d",n] = print (isDefault obj n) >> return obj
exec refTable def obj ["r",n] = print (isReadonly refTable obj n) >> return obj
exec refTable def obj ["c",n] = putStrLn (getDoc obj n) >> return obj
exec _ _ obj ["x"] = return obj
exec _ _ obj _ = putStrLn "Unknown command" >> return obj

convertValue :: String -> Value obj
convertValue "true" = Bool True
convertValue "false" = Bool False
convertValue ('"':s) = String $ init s
convertValue n = Number (read n :: Int)

usage :: IO ()
usage = do
    putStrLn "Usage"
    putStrLn "  p - print current state"
    putStrLn "  g <Name> - get a value"
    putStrLn "  s <Name> <Value> - set a value"
    putStrLn "  d <Name> - is default"
    putStrLn "  r <Name> - is readonly"
    putStrLn "  c <Name> - documentation"
    putStrLn "  x - exit"

printObj :: PropertiesObject obj => Show obj => RefTable obj -> obj -> IO ()
printObj refTable obj = do
    pPrint $ map (\n -> n ++ " = " ++ (show $ fromJust $ get refTable obj n)) $ PO.all refTable obj
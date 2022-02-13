module Generator.Gen (gen) where

import Control.Monad
import Shelly
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import Model.Definition
import Generator.Data
import Generator.Convert

gen1 :: STGroup String -> String -> GDefinitionList -> String
gen1 templates templateName definitions = render $ setAttribute "d" definitions t
  where
    Just t = getStringTemplate templateName templates

files :: [(String, String)]
files = [
    ("DefinitionXml", "PropertiesDefinition.xml"),
    ("Resolvers", "ResolverFactory.cs")
  ]

gen :: DefinitionList -> IO ()
gen x = do
        templates <- directoryGroup "src/Generator/Templates" :: IO (STGroup String)
ok        forM_ files $ \(inFile,outFile) -> writeFile ("out/TestUC/" ++ outFile) $ gen1 templates inFile definitions
        shelly $ cp_r "src/Generator/Static/." "out/TestUC"

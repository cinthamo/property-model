module Generator.Gen (gen) where

import Control.Monad
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
    ("BuildBat", "Build.bat"),
    ("BuildSln", "Cristian.TestUC.sln"),
    ("BuildProj", "Cristian.TestUC.csproj"),
    ("BuildInfo", "Properties/AssemblyInfo.cs"),
    ("Control", "TestUC.control"),
    ("DefinitionXml", "PropertiesDefinition.xml"),
    ("Resolvers", "ResolverFactory.cs")
  ]

gen :: DefinitionList -> IO ()
gen x = do
        templates <- directoryGroup "src/Generator/Templates" :: IO (STGroup String)
        let definitions = convert x
        forM_ files $ \(inFile,outFile) -> writeFile ("out/TestUC/" ++ outFile) $ gen1 templates inFile definitions

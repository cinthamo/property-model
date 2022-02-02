module Generator.Gen (gen) where

import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import Model.Definition
import Generator.Data
import Generator.Convert

gen1 :: STGroup String -> String -> [GDefinition] -> String
gen1 templates templateName definitions = render $ setAttribute "d" definitions t
  where
    Just t = getStringTemplate templateName templates

gen :: DefinitionList -> IO String
gen x = do
        templates <- directoryGroup "src/Generator/Templates" :: IO (STGroup String)
        let xml = gen1 templates "DefinitionXml" $ convert x
        return xml

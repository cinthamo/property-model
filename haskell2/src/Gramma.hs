module Gramma where

import Language.ANTLR4

data S = S1 String
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;
  
  j : ID -> S1;
  
  ID : [a-z] -> String;
  WS : [ \t\r\n]+ -> String;
|]

isWS T_WS = True
isWS _ = False

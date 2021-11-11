module Gramma where

import Language.ANTLR4

data S = S1 String | S2 String
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;
  
  j : BOOL -> S1
   | NAME -> S2;
  
  BOOL: 'true' | 'false' -> String;
  NAME: [a-zA-Z][a-zA-Z0-9_]* -> String;
  NUMBER: [0-9]+ -> Int;
  
  BLOCK_COMMENT: '/*' .* '*/' -> String;
  EOL_COMMENT: '//' [~\r\n]* -> String;
  WS: [ \n\t\r]+ -> String;
|]
{--
STRING: '"' (('\\' .) | [~"\\])* '"' -> String;
--}
isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

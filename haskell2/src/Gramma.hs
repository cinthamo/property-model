module Gramma where

import Language.ANTLR4

data Expr = Number Int |
    Bool String |
    String String |
    Null String |
    Value String |
    Field String String |
    Call String Expr [Expr] |
    OpCall String String String |
    ThisField String
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;
  
  expr:
	  NUMBER -> Number
  | BOOL -> Bool
	| STRING -> String
	| NULL -> Null
  | VALUE -> Value
	| NAME '.' NAME -> Field
	| NAME -> ThisField;

  BOOL: 'true' | 'false' -> String;
  NULL: 'null' -> String;
  VALUE: 'value' -> String;
  OP: [->=<+]+ | 'or' | 'and' | 'not' -> String;
  NAME: [a-zA-Z][a-zA-Z0-9_]* -> String;
  NUMBER: [0-9]+ -> Int;
  STRING: '"' .* '"' -> String;
  
  BLOCK_COMMENT: '/*' .* '*/' -> String;
  EOL_COMMENT: '//' (~[\r\n])* -> String;
  WS: [ \n\t\r]+ -> String;
|]
{--

  | NAME '(' expr (',' expr)* ')' -> Call
	| NAME OP NAME -> OpCall;
--}
isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

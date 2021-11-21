module Parser.PGrammar where

import Language.ANTLR4

data PDefinition = PDefinition String [Rule]
  deriving (Eq, Ord, Show)

data Rule = Rule String (Maybe PExpr) (Maybe PExpr)
  deriving (Eq, Ord, Show)

data PExpr = Number Int |
    Bool String |
    String String |
    Null String |
    Value String |
    Field String String |
    ThisField String |
    Call String PExpr [PExpr] |
    OpCall PExpr String PExpr
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;
  
  definitions: property*;

  property:
    'definition' NAME '{' rule* '}' -> PDefinition;

  rule:
    NAME exprEqual? exprIf? ';' -> Rule;

  expr:
	  NUMBER -> Number
  | BOOL -> Bool
	| STRING -> String
	| NULL -> Null
  | VALUE -> Value
	| NAME '.' NAME -> Field
	| NAME -> ThisField
  | NAME '(' expr exprComma* ')' -> Call
  | expr OP expr -> OpCall
  | '(' expr ')';

  exprComma: ',' expr;
  exprEqual: '=' expr;
  exprIf: 'if' expr;

  BOOL: 'true' | 'false' -> String;
  NULL: 'null' -> String;
  VALUE: 'value' -> String;
  OP: [->=<+]+ | 'or' | 'and' | 'not' -> String;
  NAME: [a-zA-Z][a-zA-Z0-9_]* -> String;
  NUMBER: '-'?[0-9]+ -> Int;
  STRING: '"' (~[\r\n])* '"' -> String;

  BLOCK_COMMENT: '/*' .* '*/' -> String;
  EOL_COMMENT: '//' (~[\r\n])* -> String;
  WS: [ \n\t\r]+ -> String;
|]

isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

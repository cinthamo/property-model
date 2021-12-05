module Parser.PGrammar where

import Language.ANTLR4

data PDefinitionList = PDefinitionList String [PDefinition]
  deriving (Eq, Ord, Show)

data PDefinition =
    PDefinition String [PRule] |
    PExternal String [PRule] |
    PRelated String [String]
  deriving (Eq, Ord, Show)

data PRule =
    ValueRule String [IfRule] PExpr |
    SimpleRule String (Maybe PExpr)
  deriving (Eq, Ord, Show)

data IfRule = IfRule PExpr PExpr
  deriving (Eq, Ord, Show)

data PExpr = Number Int |
    Bool String |
    String String |
    Null String |
    Value String |
    Field String String |
    ThisField String |
    Call String PExpr [PExpr] |
    Not PExpr |
    OpCall PExpr String PExpr
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;

  definitions: list*;

  list:
    'list' NAME '{' property* '}' -> PDefinitionList;

  property:
    'definition' NAME '{' rule* '}' -> PDefinition
  | 'external' NAME '{' rule* '}' -> PExternal
  | 'related' NAME nameComma* ';' -> PRelated;

  rule:
    NAME '=' exprIfMulti* expr ';' -> ValueRule
  | NAME exprIf? ';' -> SimpleRule;

  nameComma : ',' NAME;

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
  | 'not' expr -> Not
  | '(' expr ')';

  exprComma: ',' expr;
  exprIf: 'if' expr;
  exprIfMulti: expr 'if' expr '|' -> IfRule;

  BOOL: 'true' | 'false' -> String;
  NULL: 'null' -> String;
  VALUE: 'value' -> String;
  OP: [->=<+]+ | 'or' | 'and' -> String;
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

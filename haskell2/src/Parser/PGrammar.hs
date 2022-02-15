module Parser.PGrammar where

import Language.ANTLR4

data PDefinitionList = PDefinitionList String (Maybe String) [PDefinition]
  deriving (Eq, Ord, Show)

data PDefinition =
    PDefinition String [PRule]
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
    Name String |
    Field PExpr String |
    MethCall PExpr String PExpr [PExpr] |
    FuncCall String PExpr [PExpr] |
    OpCall PExpr String PExpr |
    Not PExpr
  deriving (Eq, Ord, Show)

[g4|
  grammar Test;

  definitions: list*;

  list:
    'list' NAME typeIf? '{' property* '}' -> PDefinitionList;

  typeIf: ':' NAME;

  property:
    'definition' NAME '{' rule* '}' -> PDefinition;

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
	| NAME -> Name
	| expr '.' NAME -> Field
  | NAME '(' expr exprComma* ')' -> FuncCall
  | expr '.' NAME '(' expr exprComma* ')' -> MethCall
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

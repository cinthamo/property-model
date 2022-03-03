module Parser.PGrammar where

import Language.ANTLR4

data PDefinitionList = PDefinitionList String (Maybe String) [PDefinition]
  deriving (Eq, Ord, Show)

data PDefinition =
    PDefinition [String] String String (Maybe [PRule])
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
    Call String [PExpr]
  deriving (Eq, Ord, Show)


-- TYPE NAME optType? CORCHA property* CORCHC -> pDefinitionList
pDefinitionList :: s -> String -> Maybe String -> s -> [PDefinition] -> s -> PDefinitionList
pDefinitionList _ name aType _ definitions _ = PDefinitionList name aType definitions

-- doc* NAME COLON NAME optRules? end? -> pDefinition
pDefinition :: [String] -> String -> s -> String -> Maybe [PRule] -> Maybe s -> PDefinition
pDefinition doc name _ aType rules _ = PDefinition doc name aType rules

-- CORCHA rule* CORCHC -> pRules
pRules :: s -> [PRule] -> s -> [PRule]
pRules _ rules _ = rules

-- NAME EQUAL exprIfMulti* expr end? -> valueRule
valueRule :: String -> s -> [IfRule] -> PExpr -> Maybe s -> PRule
valueRule name _ rules expr _ = ValueRule name rules expr

-- NAME exprIf?                 end? -> simpleRule;
simpleRule :: String -> Maybe PExpr -> Maybe s -> PRule
simpleRule name expr _ = SimpleRule name expr

-- optType: COLON NAME    -> skip1
-- nameComma : COMMA NAME -> skip1
-- exprComma: COMMA expr  -> skip1
-- exprIf: IF expr        -> skip1
skip1 :: s -> t -> t
skip1 _ x = x

-- exprIfMulti: expr IF expr PIPE -> ifRule
ifRule :: PExpr -> s -> PExpr -> s -> IfRule
ifRule valueExpr _ condExpr _ = IfRule valueExpr condExpr

-- expr DOT NAME -> field
field :: PExpr -> s -> String -> PExpr
field expr _ name = Field expr name

-- NAME PARA expr exprComma* PARC -> funcCall
funcCall :: String -> s -> PExpr -> [PExpr] -> s -> PExpr
funcCall name _ parm0 parmL _ = Call name (parm0:parmL)

-- expr DOT NAME PARA expr exprComma* PARC -> methCall
methCall :: PExpr -> s -> String -> s -> PExpr -> [PExpr] -> s -> PExpr
methCall target _ name _ parm0 parmL _ = Call name (target:(parm0:parmL))

-- expr OP expr -> opCall
opCall :: PExpr -> String -> PExpr -> PExpr
opCall lExpr op rExpr = Call op [lExpr, rExpr]

-- NOT expr -> pNot
pNot :: s -> PExpr -> PExpr
pNot _ expr = Call "not" [expr]

-- PARA expr PARC -> parExpr
parExpr :: s -> PExpr -> s -> PExpr
parExpr _ expr _ = expr

[g4|
  grammar Properties;

  // LEXER //

  BOOL: 'true' | 'false' -> String;
  NOT: 'not'     -> String;
  NULL: 'null'   -> String;
  VALUE: 'value' -> String;
  TYPE: 'type'   -> String;  
  IF: 'if'       -> String;

  PARA: '('      -> String;
  PARC: ')'      -> String;
  CORCHA: '{'    -> String;
  CORCHC: '}'    -> String;
  COLON: ':'     -> String;
  EQUAL: '='     -> String;
  SEMICOLON: ';' -> String;
  COMMA: ','     -> String;
  DOT: '.'       -> String;
  PIPE: '|'      -> String;

  OP: [->=<+]+ | 'or' | 'and' -> String;
  NAME: [a-zA-Z][a-zA-Z0-9_]* -> String;
  NUMBER: '-'?[0-9]+          -> Int;
  STRING: '"' (~[\r\n])* '"'  -> String;

  BLOCK_DOC: '/**' .* '*/'  -> String;
  EOL_DOC: '///' (~[\r\n])* -> String;

  BLOCK_COMMENT: '/*' .* '*/'  -> String;
  EOL_COMMENT: '//' (~[\r\n])* -> String;
  WS: [ \t\r\n]+               -> String;


  // GRAMMAR //

  definitions: type*;

  type:
    TYPE NAME optType? CORCHA property* CORCHC -> pDefinitionList;

  optType: COLON NAME -> skip1;

  property:
    doc* NAME COLON NAME optRules? end? -> pDefinition;

  doc:
    BLOCK_DOC
  | EOL_DOC;

  optRules:
    CORCHA rule* CORCHC -> pRules;

  rule:
    NAME EQUAL exprIfMulti* expr end? -> valueRule
  | NAME exprIf?                 end? -> simpleRule;

  end: SEMICOLON;

  nameComma : COMMA NAME -> skip1;

  expr:
	  NUMBER -> Number
  | BOOL -> Bool
	| STRING -> String
	| NULL -> Null
  | VALUE -> Value
	| NAME -> Name
	| expr DOT NAME -> field
  | NAME PARA expr exprComma* PARC -> funcCall
  | expr DOT NAME PARA expr exprComma* PARC -> methCall
  | expr OP expr -> opCall
  | NOT expr -> pNot
  | PARA expr PARC -> parExpr;

  exprComma: COMMA expr          -> skip1;
  exprIf: IF expr                -> skip1;
  exprIfMulti: expr IF expr PIPE -> ifRule;
|]

isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

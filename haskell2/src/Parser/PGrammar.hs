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

-- NAME EQUAL ccase* expr end? -> ruleEqual
ruleEqual :: String -> s -> [IfRule] -> PExpr -> Maybe s -> PRule
ruleEqual name _ rules expr _ = ValueRule name rules expr

-- NAME ifExpr? end? -> ruleBool
ruleBool :: String -> Maybe PExpr -> Maybe s -> PRule
ruleBool name expr _ = SimpleRule name expr

-- colonName: COLON NAME     -> skip1
-- ifExpr: IF expr           -> skip1
-- commaExpr: COMMA expr     -> skip1
skip1 :: s -> t -> t
skip1 _ x = x

-- listExpr: expr commaExpr* -> cons
cons :: PExpr -> [PExpr] -> [PExpr]
cons x l = x:l

-- exprIfMulti: expr IF expr PIPE -> ifRule
ifRule :: PExpr -> s -> PExpr -> s -> IfRule
ifRule valueExpr _ condExpr _ = IfRule valueExpr condExpr

-- expr DOT NAME -> field
field :: PExpr -> s -> String -> PExpr
field expr _ name = Field expr name

-- NAME PARA listExpr? PARC -> funcCall
funcCall :: String -> s -> Maybe [PExpr] -> s -> PExpr
funcCall name _ parmL _ = Call name (maybe [] id parmL)

-- expr DOT func -> methCall
methCall :: PExpr -> s -> PExpr -> PExpr
methCall target _ (Call name parmL) = Call name (target:parmL)

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

  // LEXER COMMON //

  BOOL: 'true' | 'false' -> String;

  PARA: '('      -> String;
  PARC: ')'      -> String;
  CORCHA: '{'    -> String;
  CORCHC: '}'    -> String;
  BRACKA: '['    -> String;
  BRACKC: ']'    -> String;
  COLON: ':'     -> String;
  EQUAL: '='     -> String;
  SEMICOLON: ';' -> String;
  COMMA: ','     -> String;
  DOT: '.'       -> String;
  PIPE: '|'      -> String;

  NAME: [a-zA-Z][a-zA-Z0-9_]* -> String;
  NUMBER: '-'?[0-9]+          -> Int;
  
  STRING: STRING_DOUBLE | STRING_SINGLE -> String;
  fragment STRING_DOUBLE: '"' IN_STRING '"';
  fragment STRING_SINGLE: ['] IN_STRING ['];
  fragment IN_STRING: .*;

  BLOCK_COMMENT: '/*' .* '*/'  -> String;
  EOL_COMMENT: '//' (~[\r\n])* -> String;
  WS: [ \n\t\r]+               -> String;


  // LEXER //
	
  NOT: 'not'     -> String;
  NULL: 'null'   -> String;
  VALUE: 'value' -> String;
  TYPE: 'type'   -> String;  
  IF: 'if'       -> String;

  MULT: [*/]     -> String;
  ADD: '+' | '-' -> String; // doesn't supoort [+-]
  COMP: [=<>]+   -> String;
  AND: 'and'     -> String;
  OR: 'or'       -> String;

  BLOCK_DOC: '/**' .* '*/'  -> String;
  EOL_DOC: '///' (~[\r\n])* -> String;
  

  // GRAMMAR //

  definitions: type*;

  type:
    TYPE NAME colonName? CORCHA property* CORCHC -> pDefinitionList;

  property:
    doc* NAME COLON NAME optRules? end? -> pDefinition;

  doc:
    BLOCK_DOC
  | EOL_DOC;

  optRules:
    CORCHA aRule* CORCHC -> pRules;

  aRule:
    NAME EQUAL ccase* expr end? -> ruleEqual
  | NAME ifExpr?           end? -> ruleBool
  ;

  end: SEMICOLON;
  ccase: expr IF expr PIPE  -> ifRule;

  colonName: COLON NAME     -> skip1;
  ifExpr: IF expr           -> skip1;
  commaExpr: COMMA expr     -> skip1;
  listExpr: expr commaExpr* -> cons;

  expr:
    NUMBER         -> Number
  | BOOL           -> Bool
  | STRING         -> String
  | NULL           -> Null
  | VALUE          -> Value
  | NAME           -> Name
  | expr DOT NAME  -> field
  | func
  | expr DOT func  -> methCall
  | expr MULT expr -> opCall
  | expr ADD expr  -> opCall
  | expr COMP expr -> opCall
  | expr AND expr  -> opCall
  | expr OR expr   -> opCall
  | NOT expr       -> pNot
  | PARA expr PARC -> parExpr
  ;
  
  func: NAME PARA listExpr? PARC -> funcCall;
|]

isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

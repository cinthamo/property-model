module Parser.PGrammar where

import Language.ANTLR4

data PDefinitionList = PDefinitionList PNameExtends [PDefinition]
  deriving (Eq, Ord, Show)

data PNameExtends =
    NoExtend String |
    ExternalExtend String |
    InternalExtend String String
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


-- TYPE nameExtends CORCHA property* CORCHC -> pDefinitionList
pDefinitionList :: s -> PNameExtends -> s -> [PDefinition] -> s -> PDefinitionList
pDefinitionList _ nameExtends _ definitions _ = PDefinitionList nameExtends definitions

-- NAME extendsType? -> pNameExtendsTwo
pNameExtendsTwo :: String -> Maybe String -> PNameExtends
pNameExtendsTwo s e = case (e) of
  Nothing -> NoExtend s
  Just t -> InternalExtend s t

-- extendsType -> pNameExtendsOne
pNameExtendsOne :: String -> PNameExtends
pNameExtendsOne t = ExternalExtend t

-- doc* NAME COLON NAME optRules? end? -> pDefinition
pDefinition :: [String] -> String -> s -> String -> Maybe [PRule] -> Maybe s -> PDefinition
pDefinition doc name _ aType rules _ = PDefinition doc name aType rules

-- CORCHA rule* CORCHC -> pRules
pRules :: s -> [PRule] -> s -> [PRule]
pRules _ rules _ = rules

-- NAME EQUAL expr ccase* end? -> ruleEqual
ruleEqual :: String -> s -> PExpr -> [IfRule] -> Maybe s -> PRule
ruleEqual name _ expr rules _ = ValueRule name rules expr

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

-- exprIfMulti: IF expr PIPE expr -> ifRule
ifRule :: s -> PExpr -> s -> PExpr -> IfRule
ifRule _ condExpr _ valueExpr = IfRule valueExpr condExpr

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

  // LEXER //
	
  NOT: 'not'     -> String;
  NULL: 'null'   -> String;
  VALUE: 'value' -> String;
  TYPE: 'type'   -> String;  
  EXTENDS: 'extends' -> String;
  IF: 'if'       -> String;

  OP: [->=<+]+ | 'or' | 'and' -> String; // Change not supported

  BLOCK_DOC: '/**' .* '*/'  -> String;
  EOL_DOC: '///' (~[\r\n])* -> String;
  
  
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
  STRING: '"' (~[\r\n])* '"'  -> String; // Change not supported


  BLOCK_COMMENT: '/*' .* '*/'  -> String;
  EOL_COMMENT: '//' (~[\r\n])* -> String;
  WS: [ \n\t\r]+               -> String;
  

  // GRAMMAR //

  definitions: type*;

  type:
    TYPE nameExtends CORCHA property* CORCHC -> pDefinitionList;

  nameExtends:
    NAME extendsType? -> pNameExtendsTwo
  |      extendsType  -> pNameExtendsOne
  ;

  property:
    doc* NAME COLON NAME optRules? end? -> pDefinition;

  doc:
    BLOCK_DOC
  | EOL_DOC;

  optRules:
    CORCHA aRule* CORCHC -> pRules;

  aRule:
    NAME EQUAL expr ccase* end? -> ruleEqual
  | NAME ifExpr?           end? -> ruleBool
  ;

  end: SEMICOLON;
  ccase: IF expr PIPE expr -> ifRule;

  extendsType: EXTENDS NAME -> skip1;
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
  | expr OP expr   -> opCall // Change not supported
  | NOT expr       -> pNot
  | PARA expr PARC -> parExpr
  ;
  
  func: NAME PARA listExpr? PARC -> funcCall;
|]

isWS T_BLOCK_COMMENT = True
isWS T_EOL_COMMENT = True
isWS T_WS = True
isWS _ = False

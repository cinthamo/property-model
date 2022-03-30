grammar PDefinitionParser;

import PDefinitionLexer;

definitions: importt* flags* type*;

importt: IMPORT name=NAME;

flags: FLAGS name=NAME open=CORCHA flagDefinition* CORCHC;

flagDefinition:
  ddoc=doc* name=NAME COLON ttype=NAME (open=CORCHA fRule* CORCHC)? end?;

fRule:
  name=NAME (EQUAL value=STRING)? end?;

type:
  TYPE nameExtends open=CORCHA property* CORCHC;

nameExtends:
  name=NAME (EXTENDS ttype=NAME)?  # nameNormal
| EXTENDS etype=NAME               # nameExternal
;

property:
  ddoc=doc* name=idPropName COLON ttype=NAME (open=CORCHA aRule* CORCHC)? end?;

propName:
  NAME
| IMPORT
| FLAGS
| TYPE
| EXTENDS
| STRING_SINGLE
;

idPropName:
  propName
| BOOL
| NOT
| NULL
| VALUE
| IF
| AND
| OR
;

doc:
  BLOCK_DOC
| EOL_DOC
;

aRule:
  name=ruleName EQUAL ccase* otherwise=expr end?   # ruleEqual
| name=ruleName (IF condition=expr)? end?          # ruleBool
;

ruleName:
  name=NAME                  # idName
| fName=NAME DOT name=NAME   # idFlag
;

end: SEMICOLON;
ccase: expr IF expr PIPE;

expr:
  value=NUMBER                  # exprNumber
| value=BOOL                    # exprBool
| value=STRING_DOUBLE           # exprString
| NULL                          # exprNull
| VALUE                         # exprValue
| name=propName                 # exprName
| target=expr DOT prop=NAME     # exprProp
| func                          # exprFunction
| target=expr DOT func          # exprMethod
| left=expr op=MULT right=expr  # exprOperator
| left=expr op=ADD right=expr   # exprOperator
| left=expr op=COMP right=expr  # exprOperator
| left=expr op=AND right=expr   # exprOperator
| left=expr op=OR right=expr    # exprOperator
| NOT expr                      # exprNot
| PARA expr PARC                # exprParenthesis
;

func: name=NAME PARA (expr (COMMA expr)*)? PARC;
  
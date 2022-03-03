parser grammar PGrammar;

options { tokenVocab=PLexer; }

definitions: list*;

list:
  LIST name=NAME (COLON type=NAME)? CORCHA property* CORCHC;

property:
  DEFINITION name=NAME CORCHA rules=aRule* CORCHC;

aRule:
  name=NAME EQUAL case* otherwise=expr SEMICOLON   # ruleEqual
| name=NAME (IF condition=expr)? SEMICOLON                    # ruleBool
;

case: expr IF expr PIPE;

expr:
  value=NUMBER                # exprNumber
| value=BOOL                  # exprBool
| value=STRING                # exprString
| NULL                        # exprNull
| VALUE                       # exprValue
| name=NAME                   # exprName
| target=expr DOT prop=NAME   # exprProp
| func                        # exprFunction
| target=expr DOT func        # exprMethod
| left=expr OP right=expr     # exprOperator
| NOT expr                    # exprNot
| PARA expr PARC              # exprParenthesis
;

func: name=NAME PARA (expr (COMMA expr)*)? PARC;
  
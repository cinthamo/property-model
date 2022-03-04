parser grammar PGrammar;

options { tokenVocab=PLexer; }

definitions: type*;

type:
  TYPE name=NAME (COLON type=NAME)? CORCHA property* CORCHC;

  property:
    doc=doc* name=NAME COLON type=NAME (CORCHA aRule* CORCHC)? end?;

  doc:
    BLOCK_DOC
  | EOL_DOC;

aRule:
  name=NAME EQUAL ccase* otherwise=expr end?   # ruleEqual
| name=NAME (IF condition=expr)? end?          # ruleBool
;

end: SEMICOLON;
ccase: expr IF expr PIPE;

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
  
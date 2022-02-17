parser grammar PropsParser;

options { tokenVocab=PropsLexer; }

definitions: list*;

list:
  LIST NAME (COLON NAME)? CORCHA property* CORCHC;

property:
  DEFINITION NAME CORCHA aRule* CORCHC;

aRule:
  NAME EQUAL case* expr SEMICOLON
| NAME (IF expr)? SEMICOLON;

case: expr IF expr PIPE;

expr:
  NUMBER
| BOOL
| STRING
| NULL
| VALUE
| NAME
| expr DOT NAME
| func
| expr DOT func
| expr OP expr
| NOT expr
| PARA expr PARC;

func: NAME PARA (expr (COMMA expr)*)? PARC;
  
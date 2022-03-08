grammar PInstParser;

import PInstLexer;

instance: object | ;

object: CORCHA (mapping (COMMA mapping)*)? CORCHC;

list: BRACKA (value (COMMA value)*)? BRACKC;

mapping: key COLON value;

key: NAME;

value:
  NUMBER  # valueNumber
| BOOL    # valueBool
| STRING  # valueString
| object  # valueObject
| list    # valueList
;

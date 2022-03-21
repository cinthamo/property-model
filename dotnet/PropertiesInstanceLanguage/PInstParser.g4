grammar PInstParser;

import PInstLexer;

instance: object | ;

object: open=CORCHA (mapping (COMMA mapping)*)? CORCHC;

list: open=BRACKA (value (COMMA value)*)? BRACKC;

mapping: key COLON value;

key:
  NAME    # keyName
| STRING  # keyString;

value:
  NUMBER  # valueNumber
| BOOL    # valueBool
| STRING  # valueString
| NAME    # valueName
| object  # valueObject
| list    # valueList
;

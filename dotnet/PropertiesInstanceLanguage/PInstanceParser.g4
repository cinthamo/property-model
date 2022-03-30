grammar PInstanceParser;

import PInstanceLexer;

instance: object | ;

object: open=CORCHA (mapping (COMMA? mapping)*)? CORCHC;

list: open=BRACKA (value (COMMA? value)*)? BRACKC;

mapping: key COLON value;

string: STRING_SINGLE | STRING_DOUBLE;

key:
  NAME    # keyName
| string  # keyString;

value:
  NUMBER  # valueNumber
| BOOL    # valueBool
| string  # valueString
| NAME    # valueName
| object  # valueObject
| list    # valueList
;

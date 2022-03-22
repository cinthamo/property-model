lexer grammar PropLexer;

import PropCommonLexer;

NOT: 'not';
NULL: 'null';
VALUE: 'value';
TYPE: 'type';
EXTENDS: 'extends';
IF: 'if';

MULT: [*/];
ADD: [+-];
COMP: '==' | '<>' | '<=' | '>=' | '<' | '>';
AND: 'and';
OR: 'or';

BLOCK_DOC: '/**' .*? '*/';
EOL_DOC: '///' (~[\r\n])*;

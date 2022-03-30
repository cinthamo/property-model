lexer grammar PDefinitionLexer;

import PCommonLexer;

NOT: 'not';
NULL: 'null';
VALUE: 'value';
IMPORT: 'import';
FLAGS: 'flags';
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

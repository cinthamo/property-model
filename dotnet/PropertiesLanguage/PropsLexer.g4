lexer grammar PropsLexer;

BOOL: 'true' | 'false';
NOT: 'not';
NULL: 'null';
VALUE: 'value';
LIST: 'list';
DEFINITION: 'definition';
IF: 'if';

PARA: '(';
PARC: ')';
CORCHA: '{';
CORCHC: '}';
COLON: ':';
EQUAL: '=';
SEMICOLON: ';';
COMMA: ',';
DOT: '.';
PIPE: '|';

OP: [->=<+]+ | 'or' | 'and';
NAME: [a-zA-Z][a-zA-Z0-9_]*;
NUMBER: '-'?[0-9]+;
STRING: '"' (~[\r\n])* '"';

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
EOL_COMMENT: '//' (~[\r\n])* -> channel(HIDDEN);
WS: [ \n\t\r]+               -> channel(HIDDEN);

lexer grammar PComonLexer;

BOOL: 'true' | 'false';

PARA: '(';
PARC: ')';
CORCHA: '{';
CORCHC: '}';
BRACKA: '[';
BRACKC: ']';
COLON: ':';
EQUAL: '=';
SEMICOLON: ';';
COMMA: ',';
DOT: '.';
PIPE: '|';

NAME: [a-zA-Z][a-zA-Z0-9_]*;
NUMBER: '-'?[0-9]+;

STRING_DOUBLE: '"' IN_STRING '"';
STRING_SINGLE: ['] IN_STRING ['];
fragment IN_STRING: .*?;

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
EOL_COMMENT: '//' (~[\r\n])* -> channel(HIDDEN);
WS: [ \n\t\r]+               -> channel(HIDDEN);

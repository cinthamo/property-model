lexer grammar PropComonLexer;

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

STRING: STRING_DOUBLE | STRING_SINGLE;

fragment STRING_DOUBLE: '"' IN_STRING '"';
fragment STRING_SINGLE: ['] IN_STRING ['];
fragment IN_STRING: .*?;

BLOCK_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
EOL_COMMENT: '//' (~[\r\n])* -> channel(HIDDEN);
WS: [ \n\t\r]+               -> channel(HIDDEN);

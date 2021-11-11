grammar Properties;

BLOCK_COMMENT: '/*' .*? '*/' -> skip;
EOL_COMMENT: '//' ~[\r\n]* -> skip;
WS: [ \n\t\r]+ -> skip;

NAME: [a-zA-Z][a-zA-Z0-9_]*;
NUMBER: [0-9]+;
BOOL: 'true' | 'false';
STRING: '"' ( ('\\' .) | ~["\\])* '"' -> String;
NULL: 'null';
VALUE: 'value';
OP: [>=<+-]+ | 'or' | 'and' | 'not';

definitions: property+;

property: 'definition' NAME '{' rule1+ '}';

rule1: rule2 ';';

rule2:
	'type' '=' NAME
	| 'default' '=' NAME condition?
	| 'apply' condition
	| 'readonly' condition
	| 'valid' condition;

condition: 'if' expr;

expr:
	NUMBER
	| BOOL
	| STRING
	| NULL
	| NAME '.' NAME
	| VALUE
	| NAME
	| NAME '(' expr (',' expr)* ')'
	| NAME OP NAME;

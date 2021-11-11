grammar Properties;

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

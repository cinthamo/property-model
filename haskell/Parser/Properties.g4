grammar Properties;

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

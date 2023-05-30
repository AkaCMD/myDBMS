/* lexer */
%option caseless
%{
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "sql.tab.h"// generate from .y
void yyerror (char const *msg);
%}

%%
SELECT				    return SELECT;
DROP					return DROP;
TABLE					return TABLE;
TABLES					return TABLES;
DATABASE				return DATABASE;
DATABASES				return DATABASES;
CREATE				    return CREATE;
INSERT				    return INSERT;
UPDATE				    return UPDATE;
SET 					return SET;
DELETE 				    return DELETE;
FROM					return FROM;
WHERE					return WHERE;
INTO 					return INTO; 
VALUES					return VALUES;
AND						return AND;
OR						return OR;
INT   					return INT;
CHAR 					return CHAR;  
SHOW					return SHOW;
EXIT					return EXIT;
USE						return USE;
[;]		|				
[(]		|				
[)]		|				
[*]		|				
[,]		|				
"<="	|
">="	|
[!]		|
"<"		|
">"		|
[=]						return *yytext; // operators
[\'][A-Za-z][A-Za-z0-9_]*[\'] 		{ yylval.str_var=strdup(yytext); return STRING; } // string value
[A-Za-z][A-Za-z0-9_]*	            { yylval.str_var=strdup(yytext); return ID; } // identifier
[0-9]+ 		                        { yylval.int_var=atoi(yytext); return NUMBER;	} // number			
\n						return *yytext;
[ \t]+					/* ignore whitespace */
%%
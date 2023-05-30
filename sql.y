%{
#include "sql.h"
// parser
char database[64]={0};
char* prompt = "sql>>";

int yylex();
int yyparse();

void yyerror (char const *msg) 
{
	fprintf(stderr, "error: %s\n", msg);
}

int yywrap() 
{
	return 1;
}

int main() 
{
	while(1){
		printf("%s", prompt);
		yyparse();
	}
	return 0;
} 

%}

%union{  
	int int_var;   
  	char *str_var;
  	struct hyper_items_def *items_var;
  	struct value_def *value_var;
  	struct item_def *item_var;
  	struct conditions_def *condition_var;
  	struct table_def *table_var;
  	struct upcon_def *update_var;
}

%token SELECT FROM WHERE AND OR DROP DELETE TABLE CREATE INTO VALUES INSERT UPDATE SET SHOW DATABASE DATABASES TABLES EXIT USE
%token <int_var> NUMBER 
%token <str_var> STRING ID INT CHAR
%type <int_var> comparator
%type <items_var> hyper_items create_items
%type <value_var> value_list value
%type <item_var> item item_list
%type <condition_var> condition conditions
%type <table_var> tables
%type <update_var> up_cond up_conds
%left OR
%left AND

%%

/* sql grammar */

line_list: line | line_list line
line: statement '\n'
statement: createsql | showsql | selectsql | insertsql | deletesql | updatesql | dropsql | exitsql | usesql

usesql: 		USE ID ';' {
					printf("\n");
					use_database($2);
					printf("\n%s", prompt);
		        }

showsql: 		SHOW DATABASES ';' {
					printf("\n");
		            show_database();
					printf("\n%s", prompt);
		        }
		        |SHOW TABLES ';' {
		        	printf("\n");
		            show_table();
					printf("\n%s", prompt);
		        }

createsql:		CREATE TABLE ID '(' hyper_items ')' ';' {
					printf("\n");
                	create_table($3, $5);
					printf("\n%s", prompt);
				}

				|CREATE DATABASE ID ';' {
					strcpy(database, $3);
					printf("\n");
					create_database();
					printf("\n%s", prompt);
				}		        

selectsql: 		SELECT '*' FROM tables ';' {
					printf("\n");
					select_where(NULL, $4, NULL);
					printf("\n");
					printf("\n%s", prompt);
				}
				| SELECT item_list FROM tables ';' {
					printf("\n");
					select_where($2, $4, NULL);
					printf("\n%s", prompt);
				}		
				|SELECT '*' FROM tables WHERE conditions ';' {
					printf("\n");
					select_where(NULL, $4, $6);
					printf("\n%s", prompt);
				}
				|SELECT item_list FROM tables WHERE conditions ';' { 
					printf("\n");
					select_where($2, $4, $6);
					printf("\n%s", prompt);
				}

deletesql:		DELETE FROM ID ';' {
					printf("\n");
					deletes($3, NULL);
					printf("\n");
					printf("\n%s", prompt);
				}

				|DELETE FROM ID WHERE conditions ';' { 
					printf("\n");
					deletes($3, $5);
					printf("\n%s", prompt);
				}


insertsql:		INSERT INTO ID VALUES '(' value_list ')' ';' {
					printf("\n");
					multi_insert($3, NULL, $6);
					printf("\n%s", prompt);
				}
		
				|INSERT INTO ID '(' item_list ')' VALUES '(' value_list ')' ';' {
					printf("\n");
					multi_insert($3, $5, $9);
					printf("\n%s", prompt);
				}


updatesql:		UPDATE ID SET up_conds ';' {
					printf("\n");
					updates($2, $4, NULL);
					printf("\n%s", prompt);
				}
		
				|UPDATE ID SET up_conds WHERE conditions ';' {
					printf("\n");
					updates($2, $4, $6);
					printf("\n%s", prompt);
				}

dropsql:		DROP TABLE ID ';' {
					printf("\n");
					drop_table($3);
					printf("\n%s", prompt);
				}
				| DROP DATABASE ID ';' {
					printf("\n");
					drop_database($3);
					printf("\n%s", prompt);
				}

exitsql: 		EXIT ';' {
					printf("\n");
		            printf("exit without error!\n");
		            exit(0);
		        }

create_items:	ID INT { // $$符号可引用产生式左部非终结符的属性值，而$i则可以引用产生式右部第i个文法符号的属性值
					$$ = (struct hyper_items_def *)malloc(sizeof(struct hyper_items_def));
                    $$->field = $1;
                    $$->type = 0;	
                    $$->next = NULL;	
				}
				| ID CHAR '(' NUMBER ')'{
					$$ = (struct hyper_items_def *)malloc(sizeof(struct hyper_items_def));
                    $$->field = $1;
                    $$->type = 1;
                    $$->next = NULL;	
				}

hyper_items: 	create_items {
					$$ = $1;
				}
				| hyper_items ',' create_items {
					$$ = $3;
					$$->next = $1;				
				}

item: 			ID {
					$$ = (struct item_def *)malloc(sizeof(struct item_def));
					$$->field = $1;
					$$->pos = NULL;
					$$->next = NULL;
				}

item_list: 		item {
					$$ = $1;				
				}
				| item_list ',' item{
					$$ = $3;
					$$->next = $1;
				}

value:			NUMBER {
					$$ = ((struct value_def *)malloc(sizeof(struct value_def)));
					$$->value.intkey = $1;
					$$->type = 0;
					$$->next = NULL;
				}
				| STRING {
					$$ = ((struct value_def *)malloc(sizeof(struct value_def)));
					strcpy($$->value.skey, $1);
					$$->type = 1;
					$$->next = NULL;
				}

value_list:		value {
					$$ = $1;
				}
				| value_list ',' value {
					$$ = $3;
					$$->next = $1;
				}

comparator:		'=' {$$ = 1; }
				| '>' {$$ = 2; }
				| '<' {$$ = 3; }
				| ">=" {$$ = 4; }
				| "<=" {$$ = 5; }
				| '!' '=' {$$ = 6; }

condition: 		item comparator NUMBER {
					$$ = ((struct conditions_def *)malloc(sizeof(struct conditions_def)));
					$$->type = 0;
					$$->litem = $1;
					$$->intv = $3;
					$$->cmp_op = $2;
					$$->left = NULL;
					$$->right = NULL;
				}
				| item comparator STRING {
					$$ = ((struct conditions_def *)malloc(sizeof(struct conditions_def)));
					$$->type = 1;
					$$->litem = $1;
					$$->strv = $3;
					$$->cmp_op = $2;
					$$->left = NULL;
					$$->right = NULL;
				}

conditions: 	condition {
					$$ = $1;
				}
				|'(' conditions ')' {
					$$ = $2;
				}
				| conditions AND conditions {
					$$ = ((struct conditions_def *)malloc(sizeof(struct conditions_def)));
					$$->cmp_op = 7;
					$$->left = $1;
					$$->right = $3;
				}
				| conditions OR conditions {
					$$ = ((struct conditions_def *)malloc(sizeof(struct conditions_def)));
					$$->cmp_op = 8;
					$$->left = $1;
					$$->right = $3;
				}

tables:			ID {
					$$ = ((struct table_def *)malloc(sizeof(struct table_def)));
					$$->table = $1;
					$$->next = NULL;
				}
				| tables ',' ID{
					$$ = ((struct table_def *)malloc(sizeof(struct table_def)));
					$$->table = $3;
					$$->next = $1;				
				}

up_cond:		ID '=' NUMBER {
					$$ = ((struct upcon_def *)malloc(sizeof(struct upcon_def)));
					$$->field = $1;
					$$->type = 0;
					$$->value.intkey = $3;
					$$->next = NULL;
				}
				| ID '=' STRING {
					$$ = ((struct upcon_def *)malloc(sizeof(struct upcon_def)));
					$$->field = $1;
					$$->type = 1;
					strcpy($$->value.skey, $3);
					$$->next = NULL;				
				}

up_conds:		up_cond {
					$$ = $1;
				}
				| up_conds ',' up_cond {
					$$ = $3;
					$$->next = $1;
				}

%%

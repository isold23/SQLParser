// @author isold.wang@gmail.com


%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "data_column.h"
#include "parser.h"
#include "expr.h"
#include "select.h"

using longyu::Parser;
using longyu::ItemColumn;
using longyu::ItemData;
using longyu::Item;
using longyu::ItemExpr;
using longyu::ItemField;
using longyu::ItemFieldEx;
using longyu::ItemSelect;
using longyu::ItemSelectList;
using longyu::ItemTableReference;
using longyu::ItemTable;
using longyu::ItemJoinList;
using longyu::ItemJoin;
using longyu::ItemJoinConditionList;
using longyu::ItemWhere;
using longyu::ItemResultList;
using longyu::ItemStringConstant;
using longyu::ItemIntegerConstant;
using longyu::ItemDoubleConstant;
using longyu::ItemResultColumn;
using longyu::ItemOperation;
using longyu::ItemParameter;
using longyu::ItemParameterList;
using longyu::ItemFunction;
using longyu::ItemTableRule;
using longyu::ItemTableRuleList;

using longyu::SUCC;
using longyu::FAIL;

extern Parser g_parser;

extern int yylex(void);
extern void yyerror(char * s, ...);

%}

%union 
{
    long long llval;
    double floatval;
    char * strval;
    int subtok;
    int fn;
    ItemColumn * p_column;
    ItemData * p_data;
    Item * p_item;
    ItemExpr* p_expr;
    ItemField* p_field;
    ItemFieldEx* p_fieldex;
    ItemSelect* p_select;
    ItemSelectList* p_select_list;
    ItemTableReference* p_table_reference;
    ItemTable* p_table;
    ItemJoinList* p_join_list;
    ItemJoin* p_join;
    ItemJoinConditionList* p_join_condition_list;
    ItemWhere* p_where;
    ItemResultList* p_result_list;
    ItemParameter* p_parameter;
    ItemParameterList* p_parameter_list;
    ItemFunction* p_function;
    ItemTableRule* p_table_rule;
    ItemTableRuleList* p_table_rule_list;
}


%token <llval> INTEGER
%token <floatval> DOUBLE
%token <strval> NAME
%token <strval> STRING
%token <fn> FUNC

%left OR
%left AND
%left '!'
%left <subtok> COMPARISON /* == != > < >= <= */
%left '|'
%left '&'
%left <subtok> SHIFT  /* << >> */
%left '+' '-'
%left '*' '/' '%'

%token AS
%token BY
%token DEFAULT
%token FROM
%token GROUP
%token INTO
%token JOIN
%token LENGTH
%token ON
%token OPT_CREATE
%token PROTOCOL
%token SELECT
%token WHERE
%token TABLE_RULE
%token STRCAT
%token CP OP SEM DOT

%type <p_column> create_column
%type <p_data> create_column_list
%type <p_field> field_factor
%type <p_fieldex> field
%type <p_expr> expr raw_expr bit_expr select_expr
%type <p_select> select_stmt
%type <p_select_list> select_expr_list
%type <p_table_reference> table_reference
%type <p_table> table_factor
%type <p_join_list> join_list
%type <p_join> join_table
%type <p_join_condition_list> join_condition_list
%type <p_where> opt_where
%type <p_result_list> into_list
%type <p_parameter> parameter
%type <p_parameter_list> parameter_list
%type <p_function> function
%type <p_table_rule> opt_table_rule
%type <p_table_rule_list> table_rule_list

%start stmt_list

%%

stmt_list: stmt ';'
 	| stmt_list stmt ';'
    | table_rule_list ';'
;

stmt: create_stmt
    | select_stmt
;

create_stmt:  
        OPT_CREATE NAME '(' create_column_list ')' PROTOCOL NAME
        {
            $4->set_name($2);
            if(FAIL == $4->set_protocol($7))
            {
                return FAIL;
            }
            if(FAIL == g_parser.add_item($4))
            {
                return FAIL;
            }
        }
;

create_column_list : 
        create_column
        {
            if(NULL == ($$ = new(std::nothrow) ItemData))
            {
                return FAIL;
            }
            if(FAIL == $$->add_column($1))
            {
                return FAIL;
            }
        }
   	|  	create_column_list ',' create_column
   		{
   			if(FAIL == $1->add_column($3))
   			{
   				return FAIL;
   			}
   			$$ = $1;
   		}
;

create_column: 
     	NAME LENGTH '(' INTEGER ')'
      	{
      		if(NULL == ($$ = new(std::nothrow) ItemColumn($1, (unsigned int)($4), false, "")))
      		{
      			return FAIL; 
      		}
      	}
    |	NAME LENGTH '(' INTEGER ')' DEFAULT STRING
        {
            if(NULL == ($$ = new(std::nothrow) ItemColumn($1, (unsigned int)($4), true, $7)))
            {
                return FAIL;
            }
        }
;

field_factor:
        NAME
     	{
     		if(NULL == ($$ = new(std::nothrow) ItemField("", "", $1)))
     		{
     			return FAIL;
     		}
     	}
    |	NAME '.' NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemField("", $1, $3)))
     		{
     			return FAIL;
     		}
        }
    |	NAME '.' NAME '.' NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemField($1, $3, $5)))
     		{
     			return FAIL;
     		}
        }
;

field:
        NAME
     	{
     		if(NULL == ($$ = new(std::nothrow) ItemFieldEx("", "", $1)))
     		{
     			return FAIL;
     		}
     	}
    |	NAME '.' NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemFieldEx("", $1, $3)))
     		{
     			return FAIL;
     		}
        }
    |	NAME '.' NAME '.' NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemFieldEx($1, $3, $5)))
     		{
     			return FAIL;
     		}
        }
;

parameter:
        /* nil */
        {
            $$ = NULL;
        }
    |   raw_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemParameter($1)))
            {
                return FAIL;
            }
        }
;

parameter_list:
        /* nil */
        {
            $$ = NULL;
        }
    |   parameter
        {
            if(NULL == ($$ = new(std::nothrow) ItemParameterList()))
            {
                return FAIL;
            }
            if(FAIL == $$->add_parameter($1))
            {
                return FAIL;
            }
        }
    |   parameter_list ',' parameter
        {
            $1->add_parameter($3);
            $$ = $1;
        }      
;

function:
        /* nil */
        {
            $$ = NULL;
        }
    |   FUNC '(' parameter_list ')'
        {
             if(NULL == ($$ = new(std::nothrow) ItemFunction($1, $3)))
             {
                return FAIL;
             }     
        }
;

expr:	
        raw_expr
        { 
            $$ = $1; 
        }
    |   bit_expr
        { 
            $$ = $1; 
        }
    |	'(' bit_expr ')'
        { 
            $$ = $2; 
        }
    |   function
        {  $$ = $1; }    
;

raw_expr: 
        /* variables */
        field
     	{
     		$$ = $1;
     	}
    |   ';'
        {
        }
    |	STRING
        {
            if(NULL == ($$ = new(std::nothrow) ItemStringConstant($1)))
            {
                return FAIL;
            }
        }
    |	INTEGER
        {
            if(NULL == ($$ = new(std::nothrow) ItemIntegerConstant($1)))
            {
                return FAIL;
            }
        }
    |	DOUBLE
        {
            if(NULL == ($$ = new(std::nothrow) ItemDoubleConstant($1)))
            {
                return FAIL;
            }
        }
;

bit_expr:
        raw_expr
        { 
            $$ = $1; 
        }
    |   '(' raw_expr ')'
        {
            $$ = $2;
        }
    |   bit_expr SHIFT bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, $2, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr COMPARISON bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, $2, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '+' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 1, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '-' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 2, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '*' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 3, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '/' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 4, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '%' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 5, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr OR bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 6, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr AND bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 7, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   bit_expr '&' bit_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($1, 9, $3)))
            {
                return FAIL;
            }
            $1->set_parent_expr($$);
            $3->set_parent_expr($$);
        }
    |   '(' bit_expr SHIFT bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, $3, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr COMPARISON bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, $3, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '+' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 1, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '-' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 2, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '*' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 3, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '/' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 4, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '%' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 5, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr OR bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 6, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr AND bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 7, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
    |   '(' bit_expr '&' bit_expr ')'
        {
            if(NULL == ($$ = new(std::nothrow) ItemOperation($2, 9, $4)))
            {
                return FAIL;
            }
            $2->set_parent_expr($$);
            $4->set_parent_expr($$);
        }
;

select_stmt:  
        SELECT select_expr_list FROM table_reference opt_where INTO into_list
        {
            if(NULL == ($$ = new(std::nothrow) ItemSelect($2, $4, $5, $7)))
            {
                return FAIL;
            }
            if(FAIL == g_parser.add_item($$))
            {
                return FAIL;
            }
        }
;

select_expr_list: 
        select_expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemSelectList))
            {
                return FAIL;
            }
            if(FAIL == $$->add_column($1))
            {
                return FAIL;
            }
        }
    |	select_expr_list ',' select_expr
        {
            if(FAIL == $1->add_column($3))
            {
                return FAIL;
            }
            $$ = $1;
        }
;

select_expr: 
        expr
;

table_reference: 
        table_factor
        {
            if(NULL == ($$ = new(std::nothrow) ItemTableReference($1)))
            {
                return FAIL;
            }
        }
  	|	join_list
  		{
  			if(NULL == ($$ = new(std::nothrow) ItemTableReference($1)))
  			{
  				return FAIL;
  			}
  		}
;

table_factor: 
        NAME 
        {
            if(NULL == ($$ = new(std::nothrow) ItemTable("", $1, "")))
            {
                return FAIL;
            }
        }
    |	NAME AS NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemTable("", $1, $3)))
            {
                return FAIL;
            }
        }
    |	NAME '.' NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemTable($1, $3, "")))
            {
                return FAIL;
            }
        }
    |	NAME '.' NAME AS NAME
        {
            if(NULL == ($$ = new(std::nothrow) ItemTable($1, $3, $5)))
            {
                return FAIL;
            }
        }
;

join_list: 
        join_table
        {
            if(NULL == ($$ = new(std::nothrow) ItemJoinList))
            {
                return FAIL;
            }
            if(FAIL == $$->add_column($1))
            {
                return FAIL;
            }
        }
    |	join_list ',' join_table
        {
            if(FAIL == $1->add_column($3))
            {
                return FAIL;
            }
            $$ = $1;
        }
;

join_table: 
        table_factor JOIN table_factor ON join_condition_list
        {
            if(NULL == ($$ = new(std::nothrow) ItemJoin($1, $3, $5)))
            {
                return FAIL;
            }
        }
;

join_condition_list: 
        field_factor '=' field_factor
        {
            if(NULL == ($$ = new(std::nothrow) ItemJoinConditionList))
            {
                return FAIL;
            }
            if(FAIL == $$->add_column($1, $3))
            {
                return FAIL;
            }
        }
    |	join_condition_list AND field_factor '=' field_factor
        {
            if(FAIL == $1->add_column($3, $5))
            {
                return FAIL;
            }
            $$ = $1;
        }
; 

opt_where: 
        /* nil */
        {
            $$ = NULL;
        }
    |	WHERE expr
 	{
 	    if(NULL == ($$ = new(std::nothrow) ItemWhere($2)))
 	    {
 		return FAIL;
 	    }
 	}
;
/*
opt_groupby:
        {
            $$ = NULL;
        }
        |   GROUP BY groupby_list opt_with_rollup
;

groupby_list: expr opt_asc_desc
        |   groupby_list ',' expr opt_asc_desc

;

opt_asc_desc:
        {
            $$ = NULL;
        }
        |   ASC
        |   DESC
;

opt_with_rollup:
        {
            $$ = NULL;
        }       
        | WITH ROLLUP
;

opt_orderby:
        {
            $$ = NULL;
        }
        |   ORDER BY groupby_list
;
*/

into_list: 
        NAME '.' NAME
        {
            ItemResultColumn* tmp = NULL;
            if(NULL == (tmp = new(std::nothrow) ItemResultColumn($1, $3)))
            {
                return FAIL;
            }
            if(NULL == ($$ = new(std::nothrow) ItemResultList))
            {
                return FAIL;
            }
            if(FAIL == $$->add_column(tmp))
            {
                return FAIL;
            }
        }
 	|	into_list ',' NAME '.' NAME
 		{
 			ItemResultColumn* tmp = NULL;
            if(NULL == (tmp = new(std::nothrow) ItemResultColumn($3, $5)))
            {
                return FAIL;
            }
 			if(FAIL == $1->add_column(tmp))
 			{
 				return FAIL;
 			}
 			$$ = $1;
 		}
;

opt_table_rule:
        /* nil */
        {
            $$ = NULL;
        }
    |   TABLE_RULE NAME '.' NAME expr
        {
            if(NULL == ($$ = new(std::nothrow) ItemTableRule($2, $4, $5)))
            {
                return FAIL;
            }
        }
;

table_rule_list:
        /* nil */
        {
            $$ = NULL;
        }
    |   opt_table_rule
        {
            if(NULL == ($$ = new(std::nothrow) ItemTableRuleList))
            {
                return FAIL;
            }
            if(FAIL == $$->add_table_rule($1))
            {
                return FAIL;
            }
            if(FAIL == g_parser.add_no_sql_item($$))
            {
                return FAIL;
            }
        }
    |   table_rule_list ',' opt_table_rule
        {
            if(FAIL == $1->add_table_rule($3))
            {
                return FAIL;
            }
            $$ = $1;
            if(FAIL == g_parser.add_no_sql_item($$))
            {
                return FAIL;
            }
        }
;

%%

void yyerror(char * s, ...)
{
    extern int yylineno;
    extern char* yytext;
    va_list ap;
    va_start(ap, s);

    fprintf(stderr, "ERROR %s:%d %s ", __FILE__, yylineno, yytext);
    vfprintf(stderr, s, ap);
    fprintf(stderr, "\n");
}

int my_yyinput(char *buf, int max_size)
{
    std::string SQL = g_parser.get_sql();
    int n = SQL.size();
    if(n)
    {
        memcpy(buf, SQL.c_str(), n);
        g_parser.clear_sql();
    }

    return n;
}


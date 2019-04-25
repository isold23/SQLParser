/*******************************************************
author isold.wang@gmail.com
*******************************************************/ 

%option noyywrap nodefault yylineno case-insensitive

%{

#include <stdarg.h>
#include <string.h>
#include "data_column.h"
#include "expr.h"
#include "select.h"

using longyu::ItemColumn;
using longyu::ItemData;
using longyu::Item;
using longyu::ItemField;
using longyu::ItemFieldEx;
using longyu::ItemExpr;
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
using longyu::ItemParameter;
using longyu::ItemParameterList;
using longyu::ItemFunction;
using longyu::ItemTableRule;
using longyu::ItemTableRuleList;

#include "longyu_yacc_tab.h"

#undef YY_INPUT
#define YY_INPUT(b,r,ms)   (r = my_yyinput(b,ms))

extern void yyerror(char *s, ...);
extern int my_yyinput(char *buf, int max_size);

%}

%%

and { return AND; }
as { return AS; }
by { return BY; }
create[ \t\n]+data { return OPT_CREATE; }
default { return DEFAULT; }
from { return FROM; }
group { return GROUP; }
into { return INTO; }
join { return JOIN; }
length { return LENGTH; }
on { return ON; }
or { return OR; }
protocol { return PROTOCOL; }
select { return SELECT; }
where { return WHERE; }
table_rule { return TABLE_RULE; }

"strcat" { yylval.fn = 100; return FUNC; }
"timestamp" { yylval.fn = 101; return FUNC; }
"strrep" { yylval.fn = 102; return FUNC; }
"add" { yylval.fn = 1; return FUNC; }
"sub" { yylval.fn = 2; return FUNC; }
"mul" { yylval.fn = 3; return FUNC; }
"div" { yylval.fn = 4; return FUNC; }

-?[0-9]+ { yylval.llval = atoll(yytext); return INTEGER; }
-?[0-9]+.[0-9]+ { yylval.floatval = atof(yytext); return DOUBLE; }
'(\\.|''|[^'\n])*' |
\"(\\.|\"\"|[^"\n])*\" { yytext[strlen(yytext)-1] = '\0'; yylval.strval = strdup(yytext+1); return STRING; }  

'(\\.|[^'\n])*$ { yyerror("Unterminated string %s", yytext); }
\"(\\.|[^"\n])*$ { yyerror("Unterminated string %s", yytext); }

[A-Za-z][A-Za-z0-9_]* { yylval.strval = strdup(yytext); return NAME; }

[-+*/%&|,;()=!.] { return yytext[0]; }

"<<" 	{ yylval.subtok = 10; return SHIFT; }
">>" 	{ yylval.subtok = 11; return SHIFT; }

"==" 	{ yylval.subtok = 13; return COMPARISON; }
"!=" 	{ yylval.subtok = 14; return COMPARISON; }
">" 	{ yylval.subtok = 15; return COMPARISON; }
">=" 	{ yylval.subtok = 17; return COMPARISON; }
"<" 	{ yylval.subtok = 16; return COMPARISON; }
"<=" 	{ yylval.subtok = 18; return COMPARISON; }

"&&" 	{ return AND; }
"||" 	{ return OR; }

"("     { return OP; }
")"     { return CP; }
";"     { return SEM; }
"."     { return DOT; }

[ \t\n] /* whitespace */
. { yyerror("mystery character '%c'", *yytext); }

%%

void my_yyflush()
{
    yy_flush_buffer(YY_CURRENT_BUFFER);
}


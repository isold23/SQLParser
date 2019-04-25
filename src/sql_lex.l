/*******************************************************
 Copyright (c) 2013 Baidu.com, Inc. All Rights Reserved
 @file cdp_lex.l
 @author wangliwei01(com@baidu.com)
 @date 2013/07/10 14:33:30
 @brief
 @不能使用双斜杠注释
*******************************************************/ 

%option noyywrap nodefault yylineno case-insensitive

%{

#include <stdarg.h>
#include <string.h>
#include "data_column.h"
#include "expr.h"
#include "select.h"

using cdp::ItemColumn;
using cdp::ItemData;
using cdp::Item;
using cdp::ItemField;
using cdp::ItemFieldEx;
using cdp::ItemExpr;
using cdp::ItemSelect;
using cdp::ItemSelectList;
using cdp::ItemTableReference;
using cdp::ItemTable;
using cdp::ItemJoinList;
using cdp::ItemJoin;
using cdp::ItemJoinConditionList;
using cdp::ItemWhere;
using cdp::ItemResultList;
using cdp::ItemStringConstant;
using cdp::ItemIntegerConstant;
using cdp::ItemDoubleConstant;
using cdp::ItemResultColumn;
using cdp::ItemParameter;
using cdp::ItemParameterList;
using cdp::ItemFunction;
using cdp::ItemTableRule;
using cdp::ItemTableRuleList;

#include "cdp_yacc_tab.h"

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


/* Definition section */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"	/* header file generated by bison */
extern YYSTYPE yylval; //YYSTYPE

	int IsString = 0;
	int lineCount = 0;
	int CommentLine = 0;
	int IsVar = 0;
	int IsThesameLine = 0;
    char inputID[100];

%}

/* Define regular expression label */
newline		[\n]
digit		[0-9]
letter		[a-zA-Z]
number     {digit}+
float      {digit}+"."{digit}+
id			{letter}+({letter}|{digit})*
ws [ \t]+

%option yylineno

%x COMMENT STRING_STATE

/* Rules section */
%%

";"    {
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
 /* Arithmetic */
"+"     {
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"-" 	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"*" 	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"/" 	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"%"     {
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"++"    {
        if(IsString == 0) {
            return INCREMENT;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"--"    {
        if(IsString == 0) {
            return DECREMENT;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Relational */
">"		{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"<"		{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
">="	{
        if(IsString == 0) {
            return GREATEQ;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"<="	{
        if(IsString == 0) {
            return LESSEQ;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"=="	{
        if(IsString == 0) {
            return EQUAL; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"!="	{
        if(IsString == 0) {
            return NOTEQUAL;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Assignment */
"="		{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"+="	{
        if(IsString == 0) {
            return ADDASSIGN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"-="	{
        if(IsString == 0) {
            return SUBASSIGN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"*="	{
        if(IsString == 0) {
            return MULASSIGN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"/="	{
        if(IsString == 0) {
            return DIVASSIGN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"%="	{
        if(IsString == 0) {
            return MODASSIGN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Logical */
"&&"	{
        if(IsString == 0) {
            return AND; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"||"	{
        if(IsString == 0) {
            return OR;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"!"		{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
 /* Delimiters */
"("	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
")"	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"{"	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"}"	{
        if(IsString == 0) {
            return yytext[0];
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
\"	{
		IsString = (IsString+1)%2;
		if(IsString == 0) {
			//printf("\t String \n");
		}
		//printf("%s \t Quotation \n", yytext);
        return yytext[0];
	}

 /* Print Keywords */	
"print"		{ 
        if(IsString == 0) {
            return PRINT;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"println"	{
        if(IsString == 0) {
            return PRINTLN; 
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Condition and Loop Keywords */
"if"		{
        if(IsString == 0) {
            return IF;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"else"		{
        if(IsString == 0) {
            return ELSE;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"for"		{
        if(IsString == 0) {
            return FOR;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Declaration Keywords */
"var"		{ 
        if(IsString == 0) {
            IsVar = 1;
            return VAR;
        }
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"int"		{ 
		if(IsVar == 1) {
			IsVar = 0;
            return INT;
		}
        if(IsString == 0) {}
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}
"float32"	{
        if(IsVar == 1) {
        IsVar = 0;
        return FLOAT;
        }
        if(IsString == 0) {}
		else {
            yylval.string = strdup(yytext);
            return STRING;
        }
}

 /* Number Constant */
{number} { 
				if(IsString == 0) {
					//printf("%s \t Number \n", yytext);
                    yylval.value.i_val = atoi(yytext); 
                    return I_CONST;
				}
				else {
					//printf("%s",yytext);
                    yylval.string = strdup(yytext);
                    return STRING;
                }
         }
{float} {
                if(IsString == 0) {
                    //printf("%s \t FloatNumber \n", yytext); 
                    yylval.value.f_val = atof(yytext);
                    return F_CONST;
                }
				else {
					//printf("%s",yytext);
                    yylval.string = strdup(yytext);
                    return STRING;
                }
        }

 /* C type Comment */
"/*"				{ BEGIN COMMENT; }
<COMMENT>[^*\n]+	{ ; }
<COMMENT>\n			{ ; }
<COMMENT>"*"		{ ; }
<COMMENT>"*/"		{ BEGIN INITIAL; }

 /* C++ type Comment */
\/\/.*	{ ; }

 /* Variable ID */
 {id}       { 
		if(IsVar == 1) {
			strcpy(inputID,yytext);
            yylval.string = strdup(yytext);
            return ID;
		}
        else if(IsString == 0) {
            yylval.string = strdup(yytext);
            return USEID;
		}
        else {
            yylval.string = strdup(yytext);
            return STRING;
        }
 }

 /* others */

\n	{ 
		lineCount++;
		if(IsThesameLine != 0)
			CommentLine = CommentLine - IsThesameLine + 1;
		IsThesameLine = 0;
        return NEWLINE;
	}
{ws}	{
		if(IsString == 0){}
		else {
//			printf("%s",yytext);
            yylval.string = strdup(yytext);
            return STRING;
        }
}
<<EOF>>     { return END; }

%%

int yywrap()
{
    return 1;
}

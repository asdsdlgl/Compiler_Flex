/*	Definition section */
%{
extern int yylineno;
extern int yylex();

void yyerror(char* msg);

#define TableSize
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* Symbol table function - you can add new function if need. */

FILE* javaFile;

char INT_TYPE = 0;
char FLOAT_TYPE = 1;
int test = 0;
int lineNumber = 0;
int IsFirst = 0;
int currentIndex = 0;
int keytype = 2;
int divbyzero = 0;
int blocknumber = 0;

int currentBlock = 0;
int stacknum = 0;
int Isdeclareassign = 0;
int Labelnum = 0;
int ENDnum = 0;
int IsErrorOccur = 0;
int IsIF = 0;
int IsFOR = 0;
int ForLabelnum = 0;
int ForENDnum = 0;
int CForENDnum = 0;
int Seminum = 0;
int SemiA = 0;
int SemiB = 0;
int SemiC = 0;
int CFOR = 0;
int MaxIf = 0;
int ifstack[100] = {};
int endstack[100] = {};
int MaxEnd = 0;

char inputID[100];
char StringID[100];
const char * keyword[] = {
	"int",
	"float32"
	""
};
typedef struct Symbol {
	int index;
	int type;
    int block;
    int stackIndex;
    union {
        int number;
        double fnumber;
    };
	char name[100];
	struct Symbol *next;		
} symBol;

typedef struct hashTable {
	struct Symbol *head;
} Node;
Node table[300];
symBol *cur;

int entryNumber(char* key);
symBol* create_symbol(char* id,int type,double number);
symBol* currentSymbol(char* id);
void insert_symbol(char* id,int type,double number);
int lookup_symbol(char* id);
int lookup_symbolRe(char* id);
void dump_symbol();
void free_symbol();


%}

/* Using union to define nonterminal and token type */
%union {
    struct
    {
        union
        {
            int i_val;
            double f_val;
        };
        char type;
    } value;
    char* string;
}

/* Token without return */
%token PRINT PRINTLN 
%token IF ELSE FOR
%token VAR NEWLINE
%token INCREMENT DECREMENT GREATEQ LESSEQ EQUAL NOTEQUAL
%token ADDASSIGN SUBASSIGN MULASSIGN DIVASSIGN MODASSIGN
%token OR AND INT FLOAT END

/* Token with return, which need to sepcify type */
%token <value> I_CONST
%token <value> F_CONST
%token <string> STRING ID USEID

/* Nonterminal with return, which need to sepcify type */
%type <value> stat
%type <value> dlcs
%type <value> factor
%type <value> expr
%type <value> term
%type <value> bracket
%type <value> print_func
%type <value> declaration
%type <value> relation
%type <value> crement
%type <value> assignment
%type <value> selection
%type <value> iteration
%type <string> str
%type <value> logical
%type <value> elsestament
%type <value> ifstament
%type <value> forstament
%type <value> semistament


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : program dlcs END { return 0;}
    |
;
dlcs
    : dlcs stat
    | stat
;
stat
    : declaration NEWLINE {
        divbyzero = 0;
     }
    | expr NEWLINE {
        divbyzero = 0;
     }
    | print_func NEWLINE {
        divbyzero = 0;
     }
    | relation NEWLINE {
        divbyzero = 0;
     }
    | assignment NEWLINE {
        divbyzero = 0;
        lineNumber++;
     }
    | selection NEWLINE {
        divbyzero = 0;
     }
    | iteration NEWLINE {
        divbyzero = 0;
     }
    | logical NEWLINE {
        divbyzero = 0;
     }
    | NEWLINE {
        divbyzero = 0;
     }
;
selection
    : ifstament '(' relation ')' '{' dlcs '}' {
        IsIF = 0;
        Labelnum--;
        fprintf(javaFile,"Label_%d:\n",ifstack[Labelnum]);
     }
    | ifstament '(' relation ')' '{' dlcs '}' elsestament '{' dlcs '}' {
        IsIF = 0;
        ENDnum--;
        fprintf(javaFile,"END_%d:\n",endstack[ENDnum]);
     }
    | ifstament '(' relation ')' '{' dlcs '}' elsestament selection {
        IsIF = 0;
        ENDnum--;
        fprintf(javaFile,"END_%d:\n",endstack[ENDnum]);
     }
;
ifstament
    : IF {
        IsIF = 1;
        ifstack[Labelnum] = MaxIf;
        MaxIf++;
    }
;
elsestament
    : ELSE {
        Labelnum--;
        endstack[ENDnum] = MaxEnd;
        MaxEnd++;
        fprintf(javaFile,"\tgoto END_%d\n",endstack[ENDnum]);
        fprintf(javaFile,"Label_%d:\n",ifstack[Labelnum]);
        ENDnum++;
    }
;
iteration
    : forstament '(' relation ')' '{' dlcs '}' {
            fprintf(javaFile,"\tgoto ForLabel_%d\n",ForLabelnum-1);
            fprintf(javaFile,"ForEND_%d:\n",ForENDnum-1);
            IsFOR = 0;
        }
    | forstament assignment semistament relation semistament expr bracket dlcs '}' {
            fprintf(javaFile,"\tgoto SemiB_%d\n",SemiB-1);
            fprintf(javaFile,"CForEND_%d:\n",CForENDnum-1);
            CFOR = 0;
    }
    | forstament assignment semistament relation semistament assignment bracket dlcs '}' {
            fprintf(javaFile,"\tgoto SemiB_%d\n",SemiB-1);
            fprintf(javaFile,"CForEND_%d:\n",CForENDnum-1);
            CFOR = 0;
    }
;
semistament
    : ';' {
        if(Seminum == 0) {
            IsFOR = 0;
            CFOR = 1;
            Seminum = 1;
            fprintf(javaFile,"SemiA_%d:\n",SemiA++);
        } else {
            Seminum = 0;
            fprintf(javaFile,"\tgoto SemiC_%d\n",SemiC);
            fprintf(javaFile,"SemiB_%d:\n",SemiB++);
        }
    }
;
forstament
    : FOR {
        IsFOR = 1;
        fprintf(javaFile,"ForLabel_%d:\n",ForLabelnum++);
    }
;
logical
    : expr AND expr {printf("AND\n");}
    | expr OR expr {printf("OR\n");}
    | '!' expr {printf("NOT\n");}
;
assignment
    : USEID '=' expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE)
                    current->number = $3.i_val;
                else if($3.type==FLOAT_TYPE) {
                    current->number = $3.f_val;
                    fprintf(javaFile,"\tf2i\n");
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
                fprintf(javaFile,"\tistore %d\n",current->stackIndex);
            }else if(current->type == FLOAT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->fnumber = $3.i_val;
                    fprintf(javaFile,"\ti2f\n");
                }else if($3.type==FLOAT_TYPE) {
                    current->fnumber = $3.f_val;
                }
                $$.type = FLOAT_TYPE;
                $$.f_val = current->fnumber;
                fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
            }
        }
     }
    | USEID ADDASSIGN expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->number = current->number + $3.i_val;
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiadd\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    current->number = current->number + $3.f_val;
                    fprintf(javaFile,"\tf2i\n");
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiadd\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
            }else if(current->type == FLOAT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->fnumber = current->fnumber + $3.i_val;
                    fprintf(javaFile,"\ti2f\n");
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfadd\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                }else if($3.type==FLOAT_TYPE) {
                    current->fnumber = current->fnumber + $3.f_val;
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfadd\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                }
                $$.type = FLOAT_TYPE;
                $$.f_val = current->fnumber;
            }
        }
     }
    | USEID SUBASSIGN expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE){
                    current->number = current->number - $3.i_val;
                    fprintf(javaFile,"\tistore %d\n",stacknum);
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiload %d\n",stacknum);
                    fprintf(javaFile,"\tisub\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    current->number = current->number - $3.f_val;
                    fprintf(javaFile,"\tf2i\n");
                    fprintf(javaFile,"\tistore %d\n",stacknum);
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiload %d\n",stacknum);
                    fprintf(javaFile,"\tisub\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
            }else if(current->type == FLOAT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->fnumber = current->fnumber - $3.i_val;
                    fprintf(javaFile,"\ti2f\n");
                    fprintf(javaFile,"\tfstore %d\n",stacknum);
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfload %d\n",stacknum);
                    fprintf(javaFile,"\tfsub\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    current->fnumber = current->fnumber - $3.f_val;
                    fprintf(javaFile,"\tfstore %d\n",stacknum);
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfload %d\n",stacknum);
                    fprintf(javaFile,"\tfsub\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                }
                $$.type = FLOAT_TYPE;
                $$.f_val = current->fnumber;
            }
        }
     }
    | USEID MULASSIGN expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->number = current->number * $3.i_val;
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\timul\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    current->number = current->number * $3.f_val;
                    fprintf(javaFile,"\tf2i\n");
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\timul\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
            }else if(current->type == FLOAT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->fnumber = current->fnumber * $3.i_val;
                    fprintf(javaFile,"\ti2f\n");
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfmul\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    current->fnumber = current->fnumber * $3.f_val;
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfmul\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                }
                $$.type = FLOAT_TYPE;
                $$.f_val = current->fnumber;
            }
        }
     }
    | USEID DIVASSIGN expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            /*if(($3.type == INT_TYPE && $3.i_val == 0)||($3.type == FLOAT_TYPE && $3.f_val == 0)) {
                printf("<<ERROR>> The divisor can't be 0  Line : %d\n",yylineno);
                IsErrorOccur = 1;
                divbyzero = 0;
                break;
            }*/
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE) {
                    //current->number = current->number / $3.i_val;
                    fprintf(javaFile,"\tistore %d\n",stacknum);
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiload %d\n",stacknum);
                    fprintf(javaFile,"\tidiv\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    //current->number = current->number / $3.f_val;
                    fprintf(javaFile,"\tf2i\n");
                    fprintf(javaFile,"\tistore %d\n",stacknum);
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiload %d\n",stacknum);
                    fprintf(javaFile,"\tidiv\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
            }else if(current->type == FLOAT_TYPE) {
                if($3.type==INT_TYPE) {
                    //current->fnumber = current->fnumber / $3.i_val;
                    fprintf(javaFile,"\ti2f\n");
                    fprintf(javaFile,"\tfstore %d\n",stacknum);
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfload %d\n",stacknum);
                    fprintf(javaFile,"\tfdiv\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    //current->fnumber = current->fnumber / $3.f_val;
                    fprintf(javaFile,"\tfstore %d\n",stacknum);
                    fprintf(javaFile,"\tfload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tfload %d\n",stacknum);
                    fprintf(javaFile,"\tfdiv\n");
                    fprintf(javaFile,"\tfstore %d\n",current->stackIndex);
                }
                $$.type = FLOAT_TYPE;
                $$.f_val = current->fnumber;
            }
        }
     }
    | USEID MODASSIGN expr {
        symBol *current;
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            current = currentSymbol($1);
            if(current->type==INT_TYPE) {
                if($3.type==INT_TYPE) {
                    current->number = current->number % $3.i_val;
                    fprintf(javaFile,"\tistore %d\n",stacknum);
                    fprintf(javaFile,"\tiload %d\n",current->stackIndex);
                    fprintf(javaFile,"\tiload %d\n",stacknum);
                    fprintf(javaFile,"\tirem\n");
                    fprintf(javaFile,"\tistore %d\n",current->stackIndex);
                } else if($3.type==FLOAT_TYPE) {
                    printf("<<ERROR>> MOD with FLOAT type ERROR  Line : %d\n",yylineno);
                    IsErrorOccur = 1;
                }
                $$.type = INT_TYPE;
                $$.i_val = current->number;
            }else if(current->type == FLOAT_TYPE) {
                printf("<<ERROR>> MOD with FLOAT type ERROR  Line : %d\n",yylineno);
                IsErrorOccur = 1;
            }
        }
     }
;
relation
    : expr '<' expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifge Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifge ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifge CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val < $3.i_val) {
                printf("true\n");
            } else {
                printf("false\n");
            }
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val < $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val < $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifge CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val < $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
    | expr '>' expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifle Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifle ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifle CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val > $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val > $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val > $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifle CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val > $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
    | expr LESSEQ expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1) {
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifgt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifgt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifgt CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val <= $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val <= $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val <= $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifgt CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val <= $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
    | expr GREATEQ expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1) {
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tiflt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tiflt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tiflt CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val >= $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val >= $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val >= $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tiflt CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val >= $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
    | expr EQUAL expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1) {
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifne Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifne ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifne CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val == $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val == $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val == $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifne CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val == $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
    | expr NOTEQUAL expr {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1) {
                IsIF = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifeq Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifeq ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tifeq CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val != $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val != $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq CForEND_%d\n",CForENDnum++);
            }
            if($1.i_val != $3.f_val)
                printf("true\n");
            else
                printf("false\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            if(IsIF == 1){
                IsIF = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq Label_%d\n",ifstack[Labelnum++]);
            }
            if(IsFOR == 1) {
                IsFOR = 0;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq ForEND_%d\n",ForENDnum++);
            }
            if(CFOR == 1) {
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfcmpl\n");
                fprintf(javaFile,"\tifeq CForEND_%d\n",CForENDnum++);
            }
            if($1.f_val != $3.i_val)
                printf("true\n");
            else
                printf("false\n");
        }
     }
;
print_func
    : PRINT '(' expr ')' {
        if($3.type == INT_TYPE) {
            //fprintf(javaFile,"\t%s ","ldc");
            //fprintf(javaFile,"%d\n",$3.i_val);
            fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
            fprintf(javaFile,"\t%s\n","swap");
            fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/print(I)V");
        } else if($3.type == FLOAT_TYPE) {
            //fprintf(javaFile,"\t%s ","ldc");
            //fprintf(javaFile,"%f\n",$3.f_val);
            fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
            fprintf(javaFile,"\t%s\n","swap");
            fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/print(F)V");
        }
     }
    | PRINTLN '(' expr ')' {
        if($3.type == INT_TYPE) {
            //fprintf(javaFile,"\t%s ","ldc");
            //fprintf(javaFile,"%d\n",$3.i_val);
            fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
            fprintf(javaFile,"\t%s\n","swap");
            fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/println(I)V");
        } else if($3.type == FLOAT_TYPE) {
            //fprintf(javaFile,"\t%s ","ldc");
            //fprintf(javaFile,"%f\n",$3.f_val);
            fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
            fprintf(javaFile,"\t%s\n","swap");
            fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/println(F)V");
        }
     }
    | PRINT '(' '"' str '"' ')' {
        fprintf(javaFile,"\t%s ","ldc");
        fprintf(javaFile,"\"%s\"\n",$4);
        fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
        fprintf(javaFile,"\t%s\n","swap");
        fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
     }
    | PRINTLN '(' '"' str '"' ')' {
        fprintf(javaFile,"\t%s ","ldc");
        fprintf(javaFile,"\"%s\"\n",$4);
        fprintf(javaFile,"\t%s\n","getstatic java/lang/System/out Ljava/io/PrintStream;");
        fprintf(javaFile,"\t%s\n","swap");
        fprintf(javaFile,"\t%s\n","invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V");
     }
;
str
    : STRING str {strcat($$,$2);}
    | STRING {strcpy($$,$1);}
;
declaration
    : VAR ID type '=' expr {
        strcpy(inputID,$2);
        if($5.type==INT_TYPE)
            insert_symbol(inputID,keytype,$5.i_val);
        else
            insert_symbol(inputID,keytype,$5.f_val);
        keytype = 2;
      }
    | VAR ID type {
        strcpy(inputID,$2);
        Isdeclareassign = 1;
        insert_symbol(inputID,keytype,0);
        Isdeclareassign = 0;
        keytype = 2;
      }
;
type
    : INT {keytype = 0;}
    | FLOAT {keytype = 1;}
    /*| VOID { $$ = $1; }*/
;
expr
    : term { $$ = $1; }
    | expr '+' term {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            $$.type = INT_TYPE;
            $$.i_val = $1.i_val + $3.i_val;
            fprintf(javaFile,"\tiadd\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val + $3.f_val;
            fprintf(javaFile,"\tfadd\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.i_val + $3.f_val;
            fprintf(javaFile,"\tfstore %d\n",stacknum);
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfload %d\n",stacknum);
            fprintf(javaFile,"\tfadd\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val + $3.i_val;
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfadd\n");
        }
    }
    | expr '-' term {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            $$.type = INT_TYPE;
            $$.i_val = $1.i_val - $3.i_val;
            fprintf(javaFile,"\tisub\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val - $3.f_val;
            fprintf(javaFile,"\tfsub\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.i_val - $3.f_val;
            fprintf(javaFile,"\tfstore %d\n",stacknum);
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfload %d\n",stacknum);
            fprintf(javaFile,"\tfsub\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val - $3.i_val;
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfsub\n");
        }
    }
;

term
    : crement { $$ = $1; }
    | term  '*' crement {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            $$.type = INT_TYPE;
            $$.i_val = $1.i_val * $3.i_val;
            fprintf(javaFile,"\timul\n");
        } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val * $3.f_val;
            fprintf(javaFile,"\tfmul\n");
        } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.i_val * $3.f_val;
            fprintf(javaFile,"\tfstore %d\n",stacknum);
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfload %d\n",stacknum);
            fprintf(javaFile,"\tfmul\n");
        } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
            $$.type = FLOAT_TYPE;
            $$.f_val = $1.f_val * $3.i_val;
            fprintf(javaFile,"\ti2f\n");
            fprintf(javaFile,"\tfmul\n");
        }
      }
    | term  '/' crement {
       /* if(($3.type == INT_TYPE && $3.i_val == 0)||($3.type == FLOAT_TYPE && $3.f_val == 0)) {
            printf("<<ERROR>> The divisor can't be 0  Line : %d\n",yylineno);
            IsErrorOccur = 1;
            divbyzero = 1;
        } else {*/
            if($1.type == INT_TYPE && $3.type == INT_TYPE) {
                if($3.i_val==0)$3.i_val = 1;
                $$.type = INT_TYPE;
                $$.i_val = $1.i_val / $3.i_val;
                fprintf(javaFile,"\tidiv\n");
            } else if($1.type == FLOAT_TYPE && $3.type == FLOAT_TYPE) {
                if($3.f_val==0)$3.f_val = 1;
                $$.type = FLOAT_TYPE;
                $$.f_val = $1.f_val / $3.f_val;
                fprintf(javaFile,"\tfdiv\n");
            } else if($1.type == INT_TYPE && $3.type == FLOAT_TYPE) {
                if($3.f_val==0)$3.f_val = 1;
                $$.type = FLOAT_TYPE;
                $$.f_val = $1.i_val / $3.f_val;
                fprintf(javaFile,"\tfstore %d\n",stacknum);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfload %d\n",stacknum);
                fprintf(javaFile,"\tfdiv\n");
            } else if($1.type == FLOAT_TYPE && $3.type == INT_TYPE) {
                if($3.i_val==0)$3.i_val = 1;
                $$.type = FLOAT_TYPE;
                $$.f_val = $1.f_val / $3.i_val;
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfdiv\n");
            }
        //}
      }
    | term  '%' crement {
        if($1.type == INT_TYPE && $3.type == INT_TYPE) {
            $$.type = INT_TYPE;
            $$.i_val = $1.i_val % $3.i_val;
            fprintf(javaFile,"\tirem\n");
        } else {
            divbyzero = 1;
            printf("<<ERROR>> MOD with Float ERROR  Line : %d\n",yylineno);
            IsErrorOccur = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
        }
    }
;

crement
    : USEID INCREMENT {
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            divbyzero = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            if(cur->type == INT_TYPE) {
                cur->number = cur->number + 1;
                $$.type = INT_TYPE;
                $$.i_val = cur->number;
                fprintf(javaFile,"\tiload %d\n",cur->stackIndex);
                fprintf(javaFile,"\tldc %d\n",1);
                fprintf(javaFile,"\tiadd\n");
                fprintf(javaFile,"\tistore %d\n",cur->stackIndex);
            } else if(cur->type == FLOAT_TYPE) {
                cur->fnumber = cur->fnumber + 1;
                $$.type = FLOAT_TYPE;
                $$.f_val = cur->fnumber;
                fprintf(javaFile,"\tfload %d\n",cur->stackIndex);
                fprintf(javaFile,"\tldc %d\n",1);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfadd\n");
                fprintf(javaFile,"\tfstore %d\n",cur->stackIndex);
            }
            blocknumber = cur -> block;
            cur = NULL;
        }
     }
    | USEID DECREMENT {
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            divbyzero = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(divbyzero == 1) {
                divbyzero = 0;
                break;
            }
            if(cur->type == INT_TYPE) {
                cur->number = cur->number - 1;
                $$.type = INT_TYPE;
                $$.i_val = cur->number;
                fprintf(javaFile,"\tiload %d\n",cur->stackIndex);
                fprintf(javaFile,"\tldc %d\n",1);
                fprintf(javaFile,"\tisub\n");
                fprintf(javaFile,"\tistore %d\n",cur->stackIndex);
            } else if(cur->type == FLOAT_TYPE) {
                cur->fnumber = cur->fnumber - 1;
                $$.type = FLOAT_TYPE;
                $$.f_val = cur->fnumber;
                fprintf(javaFile,"\tfload %d\n",cur->stackIndex);
                fprintf(javaFile,"\tldc %d\n",1);
                fprintf(javaFile,"\ti2f\n");
                fprintf(javaFile,"\tfsub\n");
                fprintf(javaFile,"\tfstore %d\n",cur->stackIndex);
            }
            blocknumber = cur -> block;
            cur = NULL;
        }
     }
    | bracket { $$ = $1; }
;
bracket
    : '(' expr ')' { $$ = $2; }
    | '{' {
        if(CFOR == 1) {
            CFOR = 0;
            fprintf(javaFile,"\tgoto SemiA_%d\n",SemiA-1);
            fprintf(javaFile,"SemiC_%d:\n",SemiC++);
        } else {
            currentBlock++;
        }
     }
    | '}' {
        free_symbol();
        currentBlock--;
     }
    | factor { $$ = $1; }
;

factor
    : I_CONST {
        $$.type = INT_TYPE;
        $$.i_val = $1.i_val; 
        fprintf(javaFile,"\tldc %d\n",$1.i_val);
      }
    | F_CONST {
        $$.type = FLOAT_TYPE;
        $$.f_val = $1.f_val;
        fprintf(javaFile,"\tldc %f\n",$1.f_val);
      }
    | USEID {
	    int tableindex = lookup_symbol($1);
    	if(tableindex == -1) {
    		printf("<<ERROR>> Undeclared Variables : %s  Line : %d\n",$1,yylineno);
            IsErrorOccur = 1;
            divbyzero = 1;
            $$.type = INT_TYPE;
            $$.i_val = 0;
		} else {
            if(cur->type == INT_TYPE) {
                $$.type = INT_TYPE;
                $$.i_val = cur->number;
                fprintf(javaFile,"\tiload %d\n",cur->stackIndex);
            } else if(cur->type == FLOAT_TYPE) {
                $$.type = FLOAT_TYPE;
                $$.f_val = cur->fnumber;
                fprintf(javaFile,"\tfload %d\n",cur->stackIndex);
            }
            blocknumber = cur -> block;
            cur = NULL;
        }
     }
;

%%

void yyerror(char* msg)
{
    printf("<<ERROR>>%s Line : %d\n",msg,yylineno);
    IsErrorOccur = 1;
    //exit(1);
}


/* C code section */
int main(int argc, char** argv)
{
    int i;
    char filename[50] = "test.j";

    if(argc == 2) {
        for(i = 0;i<strlen(argv[1]);i++) {
            if(argv[1][i]!='.') {
                filename[i] = argv[1][i];
            } else {
                filename[i] = '.';
                filename[i+1] = 'j'; 
                filename[i+2] = '\0';
                break;
            }
        }
    }

    javaFile = fopen(filename,"w");

    if(javaFile == NULL) {
        printf("fail\n");
        return 0;
    }

    fprintf(javaFile,"%s\n",".class public main");
    fprintf(javaFile,"%s\n",".super java/lang/Object");
    fprintf(javaFile,"%s\n",".method public static main([Ljava/lang/String;)V");
    fprintf(javaFile,"%s\n",".limit stack 10");
    fprintf(javaFile,"%s\n",".limit locals 10");

    yylineno = 0;

    yyparse();
    //printf("\n\n Total Line : %d\n\n",yylineno);
    //dump_symbol();

    fprintf(javaFile,"\t%s\n","return");
    fprintf(javaFile,"%s\n",".end method");

    fclose(javaFile);
    if(IsErrorOccur == 1) {
        remove(filename);
    }

    return 0;
}

int entryNumber(char* key) {
	int entry = 0;
	int i = 0;
	while(key[i]!='\0') {
		entry = ( ( entry<<4 ) + key[i]) % 300;
		i++;
	}
	return entry;
}

symBol* create_symbol(char* id,int type,double number) {
	if(IsFirst == 0) {
		memset(table,0,sizeof(table));
		//printf("Create a symbol table with TableSize %d\n",300);
		IsFirst = 1;
	}
	symBol *current = (symBol *)malloc(sizeof(symBol));
	strcpy(current->name,id);
	current->type = type;
    if(type == INT_TYPE)
        current->number = number;
    else
        current->fnumber = number;
    
    current->block = currentBlock;
	current->next = NULL;
	return current;
}
void insert_symbol(char* id,int type,double number) {
	symBol* current = create_symbol(id,type,number);

	currentIndex = entryNumber(id);

	if(lookup_symbolRe(id)!=-1) {
		printf("<<ERROR>> Redefined variables : %s  Line : %d\n",id,yylineno);
        IsErrorOccur = 1;
		free(current);
		return;
	}

    if(Isdeclareassign == 1) {
        if(type == INT_TYPE)
            fprintf(javaFile,"\tldc %d\n",0);
        else
            fprintf(javaFile,"\tldc %f\n",0.0);
    }

    if(type == INT_TYPE) {
        current->stackIndex = stacknum;
        fprintf(javaFile,"\tistore %d\n",stacknum++);
    } else {
        current->stackIndex = stacknum;
        fprintf(javaFile,"\tfstore %d\n",stacknum++);
    }

    /*if(current->block==0)
    	printf("Insert a symbol number : %s\n",id);*/

	symBol* last;
	if(table[currentIndex].head==NULL) {
		current->index = currentIndex;
		table[current->index].head = current;
		return;
	}
	else {
		for(last=table[currentIndex].head;last->next!=NULL;last=last->next) {
		}
		last->next = current;
		current->index = currentIndex;
	}
	return;
}
int lookup_symbol(char* id) {
    int blocktemp = currentBlock;
	symBol *current;
	int tableindex = entryNumber(id);
    for(blocktemp = currentBlock;blocktemp >= 0;blocktemp--) {
        for(current = table[tableindex].head;current!=NULL;current=current->next) {
		    if(strcmp(current->name,id)==0&&current->block==blocktemp) {
                cur = current;
			    return current->index;
		    }
        }
    }
	return -1;
}
int lookup_symbolRe(char* id) {
	symBol *current;
	int tableindex = entryNumber(id);
	for(current = table[tableindex].head;current!=NULL;current=current->next) {
	    if(strcmp(current->name,id)==0&&current->block==currentBlock) {
            cur = current;
		    return current->index;
		}
	}
	return -1;
}
void dump_symbol() {
	symBol *current;
	printf("\nThe Symbol Table Dump:\n");
	printf("Index	ID	Type      Data\n");
	for(int i = 0;i<300;i++) {
		for(current = table[i].head ; current!=NULL ; current=current->next) {
            if(current->type == INT_TYPE)
			    printf("%2d	%s	%s       %d \n",current->index,current->name,keyword[current->type],current->number);
            else if(current->type == FLOAT_TYPE) {
                printf("%2d	%s	%s   %f\n",current->index,current->name,keyword[current->type],current->fnumber);
            }
		}
	}
	return;
}
symBol* currentSymbol(char* id) {
	symBol *current;
    int blocktemp;
	int tableindex = entryNumber(id);
	
    for(blocktemp = currentBlock;blocktemp >= 0;blocktemp--) {
    	for(current = table[tableindex].head;current!=NULL;current=current->next) {
        	if(strcmp(current->name,id)==0&&current->block==blocktemp) {
			    return current;
		    }
        }
	}
    return NULL;
}
void free_symbol() {
	symBol *current;
    symBol *nextcurrent;
    symBol *last;
    int fir = 0;
	for(int i = 0;i<300;i++) {
        last = table[i].head;
        for(current = table[i].head ; current!=NULL ; current=current->next) {
            if(current->block == currentBlock) {
                if(current == table[i].head) {
                    table[i].head = NULL;
                } else {
                    last->next = NULL;
                    for(nextcurrent = current; nextcurrent != NULL ;) {
                        nextcurrent = current->next;
                        free(current);
                        current = nextcurrent;
                    }
                }
                break;
            }
            last = current;
        }
	}
	return;
}





%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int i=1,k=-1,l=-1;
int j=0;
char curfunc[100];
int stack[100];
int top=0;
int plist[100],flist[100];
int end[100];
int arr[10];
int ct=0,c=0,b;
int loop = 0;
int errc=0;
int tipo=0;
extern int yylineno;
%}

%token<ival> INT FLOAT VOID
%token<str> ID NUM REAL
%token WHILE IF RETURN PREPROC LE STRING PRINT FUNCION DO ARRAY ELSE STRUCT STRUCT_VAR FOR GE EQ NE INC DEC
%right '='

%type<str> asignacion asignacion1 tipoconst asignacion2
%type<ival> Tipo

%union
{
	int ival;
	char *str;
}

%%

start : Funcion start
	| PREPROC start
	| Declaracion start
	|
	;

Funcion : Tipo ID '('')' CompoundStmt {
	if ($1!=retornarfunc_tipo(ct))
	{
		printf("\nError : Tipos no coinciden : Line %d\n",printline()); errc++;
	}

	if (!(strcmp($2,"printf") && strcmp($2,"scanf") && strcmp($2,"getc") && strcmp($2,"gets") && strcmp($2,"getchar") && strcmp	($2,"puts") && strcmp($2,"putchar") && strcmp($2,"clearerr") && strcmp($2,"getw") && strcmp($2,"putw") && strcmp($2,"putc") && strcmp($2,"rewind") && strcmp($2,"sprint") && strcmp($2,"sscanf") && strcmp($2,"remove") && strcmp($2,"fflush")))
		{printf("Error : Redeclaration of %s : Line %d\n",$2,printline()); errc++;}
	else
	{
		insertar($2,FUNCION);
		insertar($2,$1);
	}
	}
        | Tipo ID '(' lista_parametro ')' CompoundStmt  {
	if ($1!=retornarfunc_tipo(ct))
	{
		printf("\nError : No coinciden los tipos : Line %d\n",printline()); errc++;
	}

	if (!(strcmp($2,"printf") && strcmp($2,"scanf") && strcmp($2,"getc") && strcmp($2,"gets") && strcmp($2,"getchar") && strcmp	($2,"puts") && strcmp($2,"putchar") && strcmp($2,"clearerr") && strcmp($2,"getw") && strcmp($2,"putw") && strcmp($2,"putc") && strcmp($2,"rewind") && strcmp($2,"sprint") && strcmp($2,"sscanf") && strcmp($2,"remove") && strcmp($2,"fflush")))
		{printf("Error : Redeclaracion de %s : Linea %d\n",$2,printline());errc++;}
	else
	{
		insertar($2,FUNCION);
		insertar($2,$1);
                for(j=0;j<=k;j++)
                {insertarp($2,plist[j]);}
                k=-1;
	}
	}
	;

lista_parametro : lista_parametro ',' parametro
               | parametro
               ;

parametro : Tipo ID {plist[++k]=$1;insertar($2,$1);insertaralcance($2,i);}
          ;

Tipo : INT
	| FLOAT
	| VOID
	;

CompoundStmt : '{' StmtList '}'
	;

StmtList : StmtList stmt
	| CompoundStmt
	|
	;

stmt : Declaracion
	| if
        | for
	| while
	| dowhile
	| RETURN tipoconst ';' {

					if(!(strspn($2,"0123456789")==strlen($2)))
						almaretorno(ct,FLOAT);
					else
						almaretorno(ct,INT);
					ct++;
				}
	| RETURN ';' {almaretorno(ct,VOID); ct++;}
        | RETURN ID ';' {
                          int sct=retornaralcance($2,stack[top-1]);	//stack[top-1] - current alcance
		          int tipo=retornartipo($2,sct);
                          if (tipo==259) almaretorno(ct,FLOAT);
                          else almaretorno(ct,INT);
                          ct++;
                         }
	| ';'
	| PRINT '(' STRING ')' ';'
	| CompoundStmt
	;

dowhile : DO CompoundStmt WHILE '(' expr1 ')' ';'
	;

if : IF '(' expr1 ')' CompoundStmt
	| IF '(' expr1 ')' CompoundStmt ELSE CompoundStmt
	;

for : FOR '(' expr1 ';' expr1 ';' expr1 ')' '{' {loop=1;} StmtList {loop=0;} '}'
     ;

while : WHILE '(' expr1 ')' '{' {loop=1;} StmtList {loop=0;} '}'
	;

expr1 : expr1 LE expr1
        | expr1 GE expr1
        | expr1 NE expr1
        | expr1 EQ expr1
        | expr1 INC
        | expr1 DEC
        | expr1 '>' expr1
        | expr1 '<' expr1
	| asignacion1
	;

asignacion : ID '=' tipoconst
	| ID '+' asignacion
	| ID ',' asignacion
	| tipoconst ',' asignacion
	| ID
	| tipoconst
	;

asignacion1 : ID '=' asignacion1
	{
		int sct=retornaralcance($1,stack[top-1]);
		int tipo=retornartipo($1,sct);
		if((!(strspn($3,"0123456789")==strlen($3))) && tipo==258)
			{printf("\nError : Tipos no coinciden : Linea %d\n",printline()); errc++;}
                else if (tipo==273)  {printf("\nError : Tipos no coinciden : Linea %d\n",printline());errc++;}
		if(!buscar($1))
		{
			int currscope=stack[top-1];
			int alcance=retornaralcance($1,currscope);
			if((alcance<=currscope && end[alcance]==0) && !(alcance==0))
				verificar_alcance_actua($1,$3,currscope);
		}
	}

	| ID ',' asignacion1
	{
		if(buscar($1))
			printf("\nVariable no declarada %s : Linea %d\n",$1,printline()); errc++;
	}
	| asignacion2
	| tipoconst ',' asignacion1
	| ID
	{
		if(buscar($1))
			{ printf("\nVariable no declarada %s : Linea %d\n",$1,printline()); errc++; }
	}
	| ID '=' ID '(' paralist ')'			//function call
        {
                int sct=retornaralcance($1,stack[top-1]);
		int tipo=retornartipo($1,sct);
                //printf("%s",$3);
                int rtype;
                rtype=retornartipof($3); int ch=0;
                //printf("%d",rtype);
		if(rtype!=tipo)
			{ printf("\nError : Error Tipos no coinciden : Linea %d\n",printline()); errc++;}
		if(!buscar($1))
		{
		  for(j=0;j<=l;j++)
                  {ch = ch+verificarp($3,flist[j],j);}
                  if(ch>0) { printf("\nError :Tipo de parámetro Error o función no declarada : Linea %d\n",printline()); errc++;}
                  l=-1;
		}
	}
	| ID '(' paralist ')'			//function call without asignacion
	{
                int sct=retornaralcance($1,stack[top-1]);
		int tipo=retornartipo($1,sct); int ch=0;
		if(!buscar($1))
		{
		  for(j=0;j<=l;j++)
                  {ch = ch+verificarp($1,flist[j],j);}
                  if(ch>0) { printf("\nError :Tipo de parámetro Error o función requerida no declarada: Linea %d\n",printline()); errc++;}
                  l=-1;
		}
                else {printf("\nNO declarado Funcion %s : Linea %d\n",$1,printline());errc++;}
	}
/*	| ID '[' ID ']' '=' ID
	{
        	int sct=retornaralcance($1,stack[top-1]); 
		int itype=retornartipo($3,sct);
                int tipo=retornartipo2($1,sct); int ch=0;
                int rtype=retornartipo($6,sct);
                if(itype!=258)
                        { printf("\nError : El array debe ser de tipo Int : Linea %d\n",printline());errc++;}
		if(rtype!=tipo)
			{ printf("\nError : No Coinciden los Tipos : Linea %d\n",printline()); errc++;}
	}*/
	| tipoconst
	;

paralist : paralist ',' param
         | param
         ;

param : ID
	{
                if(buscar($1))
	        	{printf("\nVaariable no Declarada %s : Line %d\n",$1,printline());errc++;}
                else
                {
                	int sct=retornaralcance($1,stack[top-1]);
                	flist[++l]=retornartipo($1,sct);
                }
	}
	;

asignacion2 : ID '=' exp {c=0;}
		| ID '=' '(' exp ')'
		;

exp : ID
	{
		if(c==0)							//check compatibility of mathematical operations
		{
			c=1;
			int sct=retornaralcance($1,stack[top-1]);
			b=retornartipo($1,sct);
		}
		else
		{
			int sct1=retornaralcance($1,stack[top-1]);
			if(b!=retornartipo($1,sct1)){}
				{printf("\nError : Tipos no Coinciden : Linea %d\n",printline());errc++;}
		}
	}
	| exp '+' exp
	| exp '-' exp
	| exp '*' exp
	| exp '/' exp
	| '(' exp '+' exp ')'
	| '(' exp '-' exp ')'
	| '(' exp '*' exp ')'
	| '(' exp '/' exp ')'
	| tipoconst
	;

tipoconst : NUM
	| REAL
	;

Declaracion : Tipo ID '=' tipoconst ';'
	{
		if( (!(strspn($4,"0123456789")==strlen($4))) && $1==258)
			{printf("\nError : Tipos no coinciden : Linea %d\n",printline());errc++;}
                else if ($1==273)  {printf("\nError : Tipos no coinciden : Linea %d\n",printline());errc++;}
		if(!buscar($2))
		{
			int currscope=stack[top-1];
			int previous_scope=retornaralcance($2,currscope);
			if(currscope==previous_scope)
				{printf("\nError : Redeclaracion of %s : Line %d\n",$2,printline());errc++;}
			else
			{
				insertar_dup($2,$1,currscope);
				verificar_alcance_actua($2,$4,stack[top-1]);
			}
		}
		else
		{
			int alcance=stack[top-1];
			insertar($2,$1);
			insertaralcance($2,alcance);
			verificar_alcance_actua($2,$4,stack[top-1]);
		}
	}
	| asignacion1 ';'
	{
		if(!buscar($1))
		{
			int currscope=stack[top-1];
			int alcance=retornaralcance($1,currscope);
			int tipo=retornartipo($1,alcance);
			if(!(alcance<=currscope && end[alcance]==0) || alcance==0 && tipo!=271)
				{printf("\nError : Variable %s fuera de alcanze : Linea %d\n",$1,printline());errc++;}
		}
		else
			{printf("\nError : Variable no declarada%s : Linea %d\n",$1,printline());errc++;}
	}
        | Tipo ID ';'
        {
        	if(!buscar($2))
		{
			int currscope=stack[top-1];
			int previous_scope=retornaralcance($2,currscope);
			if(currscope==previous_scope)
				{printf("\nError : Redeclaracion of %s : Linea %d\n",$2,printline());errc++;}
			else
			{
				insertar_dup($2,$1,currscope);
				//verificar_alcance_actua($2,$4,stack[top-1]);
			}
		}
		else
		{
			int alcance=stack[top-1];
			//printf("%d",tipo);
			insertar($2,$1);
			insertaralcance($2,alcance);
			//verificar_alcance_actua($2,$4,stack[top-1]);
		}
	}
	| Tipo ID '[' asignacion ']' ';' {
                       int itype;
                       if(!(strspn($4,"0123456789")==strlen($4))) { itype=259; } else itype = 258;
                       if(itype!=258)
                       { printf("\nError : el array debe de ser del tipo int : Line %d\n",printline());errc++;}
                       if(atoi($4)<=0)
                       { printf("\nError : Array debe  de ser del tipo int > 0 : Line %d\n",printline());errc++;}
                       if(!buscar($2))
		       {
			int currscope=stack[top-1];
			int previous_scope=retornaralcance($2,currscope);
			if(currscope==previous_scope)
				{printf("\nError : Redeclaration of %s : Line %d\n",$2,printline());errc++;}
			else
			{

				insertar_dup($2,ARRAY,currscope);
                                insertar_por_alcance($2,$1,currscope);	//to insertar tipo to the correct identifier in case of multiple entries of the identifier by using alcance
                                if (itype==258) {insertar_index($2,$4);}
			}
		      }
		      else
		      {
			int alcance=stack[top-1];
                        insertar($2,ARRAY);
			insertar($2,$1);
			insertaralcance($2,alcance);
                        if (itype==258) {insertar_index($2,$4);}
		      }

		    }
	| STRUCT ID '{' Declaracion '}' ';' {
						insertar($2,STRUCT);
						}
	| STRUCT ID ID ';' {
				insertar($3,STRUCT_VAR);
				}
	| error
	;



%%

#include "lex.yy.c"
#include<ctype.h>
int main(int argc, char *argv[])
{
	yyin =fopen(argv[1],"r");
	if(!yyparse()&& errc<=0)
	{
		printf("\nParsing Completado\n");
		mostrar();
	}
	else
	{
		printf("\nParsing fallo\n");
                mostrar();
	}
	fclose(yyin);
	return 0;
}



struct Tsimbolo
{
	int sno;
	char token[100];
	int tipo[100];
	int paratipo[100];
	int tn;
	int pn;
	float valorf;
	int index;
	int alcance;
}tSimbolo[100];
int n=0,arr[10];
int tnp;

int retornarfunc_tipo(int ct)
{
	return arr[ct-1];
}
void almaretorno( int ct, int retornartipo )
{
	arr[ct] = retornartipo;
	return;
}
void insertaralcance(char *a,int s)
{
	int i;
	for(i=0;i<n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token))
		{
			tSimbolo[i].alcance=s;
			break;
		}
	}
}
int retornaralcance(char *a,int cs)
{
	int i;
	int max = 0;
	for(i=0;i<=n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token) && cs>=tSimbolo[i].alcance)
		{
			if(tSimbolo[i].alcance>=max)
				max = tSimbolo[i].alcance;
		}
	}
	return max;
}
int buscar(char *a)
{
	int i;
	for(i=0;i<n;i++)
	{
		if( !strcmp( a, tSimbolo[i].token) )
			return 0;
	}
	return 1;
}
int retornartipo(char *a,int sct)
{
	int i;
	for(i=0;i<n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token) && tSimbolo[i].alcance==sct)
			return tSimbolo[i].tipo[0];
	}
}

int retornartipo2(char *a,int sct)
{
	int i;
	for(i=0;i<n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token) && tSimbolo[i].alcance==sct)
			{ return tSimbolo[i].tipo[1];}
	}
}

int retornartipof(char *a)
{
	int i;
	for(i=0;i<n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token))
			{ return tSimbolo[i].tipo[1];}
	}
}


void verificar_alcance_actua(char *a,char *b,int sc)
{
	int i,j,k;
	int max=0;
	for(i=0;i<=n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token)   && sc>=tSimbolo[i].alcance)
		{
			if(tSimbolo[i].alcance>=max)
				max=tSimbolo[i].alcance;
		}
	}
	for(i=0;i<=n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token)   && max==tSimbolo[i].alcance)
		{
			float temp=atof(b);
			for(k=0;k<tSimbolo[i].tn;k++)
			{
				if(tSimbolo[i].tipo[k]==258)
					tSimbolo[i].valorf=(int)temp;
				else
					tSimbolo[i].valorf=temp;
			}
		}
	}
}
void almavalor(char *a,char *b,int s_c)
{
	int i;
	for(i=0;i<=n;i++)
	{
		if(!strcmp(a,tSimbolo[i].token) && s_c==tSimbolo[i].alcance)
		{
			tSimbolo[i].valorf=atof(b);
		}
	}
}

void insertar(char *name, int tipo)
{
	int i;
	if(buscar(name))
	{
		strcpy(tSimbolo[n].token,name);
		tSimbolo[n].tn=1;
                tSimbolo[n].pn=0;
		tSimbolo[n].tipo[tSimbolo[n].tn-1]=tipo;
		tSimbolo[n].sno=n+1;
		n++;
	}
	else
	{
		for(i=0;i<n;i++)
		{
			if(!strcmp(name,tSimbolo[i].token))
			{
				tSimbolo[i].tn++;
				tSimbolo[i].tipo[tSimbolo[i].tn-1]=tipo;
				break;
			}
		}
	}

	return;
}

void insertar_dup(char *name, int tipo, int s_c)
{
	strcpy(tSimbolo[n].token,name);
	tSimbolo[n].tn=1;
        tSimbolo[n].pn=0;
	tSimbolo[n].tipo[tSimbolo[n].tn-1]=tipo;
	tSimbolo[n].sno=n+1;
	tSimbolo[n].alcance=s_c;
	n++;
	return;
}

void insertar_por_alcance(char *name, int tipo, int s_c)
{
 	int i;
	for(i=0;i<n;i++)
 	{
  		if(!strcmp(name,tSimbolo[i].token) && tSimbolo[i].alcance==s_c)
  		{
   			tSimbolo[i].tn++;
   			tSimbolo[i].tipo[tSimbolo[i].tn-1]=tipo;
  		}
 	}
}

void insertarp(char *name,int tipo)
{
 	int i;
 	for(i=0;i<n;i++)
 	{
  		if(!strcmp(name,tSimbolo[i].token))
  		{
   			tSimbolo[i].pn++;
   			tSimbolo[i].paratipo[tSimbolo[i].pn-1]=tipo;
   			break;
  		}
 	}
}

void insertar_index(char *name,int ind)
{
 	int i;
 	for(i=0;i<n;i++)
 	{
  		if(!strcmp(name,tSimbolo[i].token) && tSimbolo[i].tipo[0]==273)
  		{
   			tSimbolo[i].index = atoi(ind);
  		}
	}
}

int verificarp(char *name,int flist,int c)
{
 	int i,j;
 	for(i=0;i<n;i++)
 	{
  		if(!strcmp(name,tSimbolo[i].token))
  		{
    			if(tSimbolo[i].paratipo[c]!=flist)
    			return 1;
  		}
 	}
 	return 0;
}

void mostrar()
{
	int i,j;
	printf("\n");
	printf("-------------------------------------------------------Tabla de Simboloe-----------------------------------------------------------\n");
	printf("------------------------------------------------------------------------------------------------------------------------------\n");
	printf("\nSNo\tIdentificador\tAlcance\t\tValor\t\tTipo\t\t\tTipo de Parametro(for functions)\n");
	printf("------------------------------------------------------------------------------------------------------------------------------\n\n");
	for(i=0;i<n;i++)
	{
		if(tSimbolo[i].tipo[0]==258 || tSimbolo[i].tipo[1]==258 || tSimbolo[i].tipo[1]==260)
			printf("%d\t%s\t\t%d\t\t%d\t",tSimbolo[i].sno,tSimbolo[i].token,tSimbolo[i].alcance,(int)tSimbolo[i].valorf);
		else
			printf("%d\t%s\t\t%d\t\t%.2f\t",tSimbolo[i].sno,tSimbolo[i].token,tSimbolo[i].alcance,tSimbolo[i].valorf);
                printf("\t");
		for(j=0;j<tSimbolo[i].tn;j++)
		{
			if(tSimbolo[i].tipo[j]==258)
				printf("INT");
			else if(tSimbolo[i].tipo[j]==259)
				printf("FLOAT");
			else if(tSimbolo[i].tipo[j]==271)
				printf("FUNCTION");
			else if(tSimbolo[i].tipo[j]==273)
				printf("ARRAY");
			else if(tSimbolo[i].tipo[j]==260)
				printf("VOID");
                        if(tSimbolo[i].tn>1 && j<(tSimbolo[i].tn-1))printf(" - ");
		}
                printf("\t\t");
		for(j=0;j<tSimbolo[i].pn;j++)
		{
			if(tSimbolo[i].paratipo[j]==258)
				printf("INT");
			else if(tSimbolo[i].paratipo[j]==259)
				printf("FLOAT");
			if(tSimbolo[i].pn>1 && j<(tSimbolo[i].pn-1))printf(", ");
		}
		printf("\n");
	}
	printf("------------------------------------------------------------------------------------------------------------------------------\n\n");
	return;
}





yyerror(char *s)
{
	printf("\nLinea %d : %s %s\n",yylineno,s,yytext);
}

int printline()
{
	return yylineno;
}
void push()
{
	stack[top]=i;
	i++;
	top++;
	return;
}
void pop()
{
	top--;
	end[stack[top]]=1;
	stack[top]=0;
	return;
}         


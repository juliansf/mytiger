#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "gc.h"
#define wSz 8

long *_createArray(long init, long size)
{
	int i;
	long *p = (long *)GC_MALLOC((size+1) * wSz);
	if(p == NULL){
		printf("Runtime error: out of memory\n");
		exit(1);
	}
	p[0] = size;
	for(i=0; i < size; i++)
		p[i+1] = init;
	return p+1;
}

long *_checkIndex(long *arr, long idx)
{
	if(idx < 0 || idx >= arr[-1]){
		printf("Runtime error: indice fuera de rango!\n");
		exit(1);
	}
	return arr+idx;
}

long *_createRecord(long nfields, ...)
{
	va_list va;
	int i;
	
	long *p = (long *) GC_MALLOC(nfields * wSz);

	if(p == NULL){
		printf("Runtime error: out of memory\n");
		exit(1);
	}
	va_start(va, nfields);
	for(i=0; i < nfields; i++)
		p[i] = va_arg(va,long);
	va_end(va);
	return p;
}

void _checkNil(long *p)
{
	if(p == NULL){
		printf("Runtime error: Referencia a record nil!\n");
		exit(1);
	}
}

typedef struct string {long cto; unsigned char string[0];} tigerString;

tigerString empty={0,""};

void print(tigerString *s)
{
	int i;
	unsigned char* p=(unsigned char*)s->string;	 
	for(i=0;i<s->cto;i++,p++) 
		putchar(*p); 
}

void printInt(long i)
{
	printf("%ld",i);
}

main (){
	extern int _tigermain(long);
	long sl;
	GC_INIT();
	_tigermain ((long)&sl);		/*Calculamos el static link de main*/
}

void flush()
{
	fflush(stdout);
}

tigerString *getstr()
{
	int c;
	tigerString *ret;
	if( (c=getchar()) == EOF ){
		return &empty;
	}else{
		ret = (tigerString *)GC_MALLOC(sizeof(tigerString));
		ret->cto=1;
		ret->string[0] = c;
		return ret;
	}
}		

long ord(tigerString *s)
{
	if(s->cto == 0)
		return -1;
	else
		return (long)(s->string[0]);
}

tigerString *chr(long i)
{
	tigerString *ret;
	if (i<0 || i>=256) {
		printf("Runtime error: chr(%ld) out of range\n",i);
		exit(1);
 	}
 	ret = (tigerString *)GC_MALLOC(sizeof(tigerString));
 	ret->cto = 1;
 	ret->string[0] = i;
 	return ret;
}

long size(tigerString *s)
{
	return s->cto;
}

tigerString *substring(tigerString *s, long first, long n)
{
	tigerString *ret;
	int i;
	if (first < 0 || first + n > s->cto){
		printf("Runtime error: substring([%ld],%ld,%ld) out of range\n",s->cto,first,n);
		exit(1);
	}
	ret = (tigerString *)GC_MALLOC(sizeof(tigerString) + n);

	ret->cto = n;
	for(i=0;i<n;i++)
		ret->string[i] = s->string[first+i];

	return ret;
}

tigerString *concat(tigerString *s1, tigerString *s2)
{
	tigerString *ret;
	ret = (tigerString *)GC_MALLOC (sizeof(tigerString) + s1->cto + s2->cto);
	ret->cto = s1->cto + s2->cto;
	memcpy(ret->string, s1->string, s1->cto);
	memcpy(ret->string + s1->cto, s2->string, s2->cto);
	return ret;
}

enum {EQ,NEQ,LT,LE,GT,GE};

long _compString (long oper, tigerString* a, tigerString* b)
{
	long res;
	int n;
	char* l = (char*) a->string;
	char* r = (char*) b->string;

	n = (a->cto < b->cto) ? a->cto : b->cto;
	
	if (n==0) {
		if (a->cto==b->cto) res=0;
		else if (a->cto==0 && b->cto!=0) res=-1;
				 else if (a->cto!=0 && b->cto==0) res=1;
	}
	else res = strncmp(l,r,n);
	
	switch (oper) {
	case EQ:
			if (res==0) return 1;
			else return 0;
			break;
	case NEQ:
			if (res!=0) return 1;
			else return 0;
			break;
	case LT:
			if (res<0) return 1;
			else return 0;
			break;
	case LE:
			if (res<=0) return 1;
			else return 0;
			break;
	case GT:
			if (res>0) return 1;
			else return 0;
			break;
	case GE:
			if (res>=0) return 1;
			else return 0;
			break;
	}
	printf("Runtime error: error interno en _compString\n");
	exit(-1);
}

long not(long i)
{ 
	return !i;
}

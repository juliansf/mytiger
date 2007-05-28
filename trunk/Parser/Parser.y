%{
	open TigerAbs
	open TigerError
	
	fun mergeDecs (TypeDec [t], TypeDec tl::decs) = TypeDec (t::tl) :: decs
		| mergeDecs (FunctionDec [f], FunctionDec fl::decs) = FunctionDec (f::fl) :: decs
		| mergeDecs (dec, decs) = dec::decs
	
	fun name { data=SimpleVar n, pos } = n
		|	name { data, pos } = Error( ErrorInvalidTypeName, pos )
%}

%token EOF
%token<int> TYPE ARRAY OF VAR FUNCTION
%token<int> LET IN END IF THEN ELSE WHILE DO FOR TO BREAK
%token<int> DOT COMMA COLON SEMICOL ASS NIL
%token<int> EQ NEQ GT GEQ LT LEQ AMPERSAND PIPE
%token<int> LP RP LB RB LSB RSB
%token<int> PLUS MINUS TIMES DIV
%token<{ data:int, pos:int }> NUM
%token<{ data:string, pos:int }> STRING
%token<{ data:string, pos:int }> ID

%type<TigerAbs.exp> prog
%type<TigerAbs.ty> ty
%type<{ data:string, pos:int }> id
%type<TigerAbs.field> tyfield
%type<TigerAbs.field list> tyfields
%type<TigerAbs.exp> exp
%type<TigerAbs.exp list> explist
%type<(TigerAbs.symbol * TigerAbs.exp) list> record_fields
%type<TigerAbs.exp list> args
%type<{ data:TigerAbs.var, pos:int }> lvalue
%type<TigerAbs.dec> dec
%type<TigerAbs.dec> fundec
%type<TigerAbs.dec> tydec
%type<TigerAbs.dec> vardec
%type<TigerAbs.dec list> decs
%type<TigerAbs.dec list> decls

%nonassoc THEN
%left ELSE
%nonassoc ASS
%left PIPE
%left AMPERSAND
%nonassoc EQ NEQ GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIV
%right UMINUS
%nonassoc DO
%nonassoc OF      

%start prog

%%
prog:
		exp EOF												{ $1 }
	| EOF														{ raise ErrorEmptyFile (!progName) }
;

exp:
		LP exp RP											{ $2 }
	|	lvalue												{ VarExp (#data($1), #pos($1)) }
	|	LP RP													{ UnitExp $1 }
	| NIL														{ NilExp $1 }
	|	NUM														{ IntExp (#data($1), #pos($1)) }	
	| STRING												{ StringExp ( #data($1), #pos($1)) }
	| id LP args RP									{ CallExp ({ func= #data($1), args=$3 }, #pos($1)) }
	| exp PLUS exp									{ OpExp ({ left=$1, oper=Plus, right=$3 }, $2) }
	| exp MINUS exp									{ OpExp ({ left=$1, oper=Minus, right=$3 }, $2) }
	| exp TIMES exp									{ OpExp ({ left=$1, oper=Times, right=$3 }, $2) }
	| exp DIV exp										{ OpExp ({ left=$1, oper=Div, right=$3 }, $2) }
	| exp EQ exp										{ OpExp ({ left=$1, oper=Eq, right=$3 }, $2) }
	| exp NEQ exp										{ OpExp ({ left=$1, oper=Neq, right=$3 }, $2) }
	| exp GT exp										{ OpExp ({ left=$1, oper=Gt, right=$3 }, $2) }
	| exp GEQ exp										{ OpExp ({ left=$1, oper=Geq, right=$3 }, $2) }
	| exp LT exp										{ OpExp ({ left=$1, oper=Lt, right=$3 }, $2) }
	| exp LEQ exp										{ OpExp ({ left=$1, oper=Leq, right=$3 }, $2) }
	| MINUS exp	%prec UMINUS				{ OpExp ({ left=IntExp(0, $1), oper=Minus, right=$2 }, $1) }
	| id LB record_fields RB				{	RecordExp ({ fields=$3, typ= #data($1) }, #pos($1)) }
	| LP exp SEMICOL explist RP			{ SeqExp ($2::$4, $1) }
	|	lvalue ASS exp								{ AssignExp ({ var= #data($1), exp=$3}, #pos($1)) }
	| IF exp THEN exp								{ IfExp ({ test=$2, then'=$4, else'=NONE, oper=If }, $1) }
	| IF exp THEN exp ELSE exp			{ IfExp ({ test=$2, then'=$4, else'=SOME $6, oper=If }, $1) }
	|	exp AMPERSAND exp							{ IfExp ({ test=$1, then'=$3, else'=SOME (IntExp (0, $2)), oper=And }, $2) }
	|	exp PIPE exp									{ IfExp ({ test=$1, then'=IntExp (1, $2), else'=SOME $3, oper=Or }, $2) }
	| WHILE exp DO exp							{ WhileExp ({ test=$2, body=$4 }, $1) }
	| FOR id ASS exp TO exp DO exp	{ ForExp ({ var= #data($2), escape=ref false, lo=$4, hi=$6, body=$8 }, $1) }
	| LET decs IN END								{ LetExp  ({ decs=$2, body=UnitExp $3 }, $1) }
	| LET decs IN exp END						{ LetExp  ({ decs=$2, body=$4 }, $1) }
	| LET decs IN exp SEMICOL explist END 
																	{ LetExp  ({ decs=$2, body=SeqExp ($4::$6, $3)}, $1) }
	|	BREAK													{ BreakExp $1 }
	| lvalue LSB exp RSB OF exp			{ ArrayExp ({ typ=name $1, size=$3, init=$6 }, #pos($1)) }
;

explist:
		exp SEMICOL explist						{ $1::$3 }
	|	exp														{ [$1] }
;

record_fields:
		id EQ exp COMMA record_fields	{ ( #data($1), $3)::$5 }
	|	id EQ exp											{ [( #data($1), $3)] }
	|																{ [] }
;

decs:
		dec decls											{ mergeDecs ($1, $2) }
;

decls:
		dec decls											{ mergeDecs ($1, $2) }
	|																{ [] }
;

dec: 
		fundec												{ $1 }
	|	vardec												{ $1 }
	| tydec													{ $1 }
;

fundec:
		FUNCTION id LP tyfields RP EQ exp
																	{ FunctionDec [({ name= #data($2), params=$4, result=NONE, body=$7 }, #pos($2))] }
	|	FUNCTION id LP tyfields RP COLON id EQ exp
																	{ FunctionDec [({ name= #data($2), params=$4, result=SOME (#data($7)), body=$9 }, 
																									#pos($2))] }
;

vardec:
		VAR id ASS exp								{ VarDec ({ name= #data($2), escape=ref false, typ=NONE, init=$4 }, $1) }
	| VAR id COLON id ASS exp				{ VarDec ({ name= #data($2), escape=ref false, typ=SOME (#data($4)), init=$6 }, $1) }
;

tydec:
		TYPE id EQ ty									{ TypeDec [({ name= #data($2), ty=$4 }, $1)] }
;

ty:
		id														{ NameTy  (#data($1)) }
	|	ARRAY OF id										{ ArrayTy (#data($3)) }
	|	LB tyfields RB								{ RecordTy $2 }
;

tyfields:
		tyfield COMMA tyfields				{ $1::$3 }
	|	tyfield												{ [$1] }
	|																{ [] }
;

tyfield:
		id COLON id										{ { name= #data($1), escape=ref false, typ= #data($3) } }

id:
		ID														{ $1 }
;

args:
		exp COMMA args								{ $1:: $3 }
	|	exp														{ [$1] }
	|																{ [] }
;

lvalue:
		id														{ { data=SimpleVar (#data($1)), pos= #pos($1) } }
	|	lvalue DOT id									{ { data=FieldVar (#data($1), #data($3)), pos= #pos($3) } }
	| lvalue LSB exp RSB						{ { data=SubscriptVar (#data($1), $3), pos= #pos($1) } }
;
%%
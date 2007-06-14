structure TigerTree =
struct
	datatype exp = 
		CONST of int
	|	NAME of TigerTemp.label
	|	TEMP of TigerTemp.temp
	|	BINOP of binop * exp * exp
	|	MEM of exp
	|	CALL of exp * exp list
	| ESEQ of stm * exp
	
	and stm = 
		MOVE of exp * exp
	| JUMP of exp * TigerTemp.label list
	| EXP of exp
	| CJUMP of relop * exp * exp * TigerTemp.label * TigerTemp.label
	| SEQ of stm * stm
	| LABEL of TigerTemp.label
	
	and binop = 
		PLUS | MINUS | MUL | DIV
	| AND | OR | XOR
	| LSHIFT | RSHIFT | ARSHIFT
	
	and relop =
		EQ | NE | LT | LE | GT | GE (* signed *)
	| ULT | ULE | UGT | UGE (* unsigned *)
end
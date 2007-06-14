signature TigerTranslate =
sig

	(*Manejo de frames y levels*)
	type level
	type access
		
	val outermost : level
	val newLevel : level * TigerTemp.label * bool list -> level
	
	val formals : level -> access list
	val allocLocal : level -> bool -> access
	
	(*Traduccion a codigo intermedio*)
	datatype exp = (* TEMPORAL! Despues hay que cambiarlo a "type exp" *)
		NN
	| Ex of TigerTree.exp (* Expresion *)
	| Nx of TigerTree.stm (* No Result *)
	| Cx of TigerTemp.label * TigerTemp.label -> TigerTree.stm (* Condicion *)
	
	val unEx : exp -> TigerTree.exp
	val unNx : exp -> TigerTree.stm
	val unCx : exp -> TigerTemp.label * TigerTemp.label -> TigerTree.stm
	
	val seq : TigerTree.stm list -> TigerTree.stm
	(*Una funci'on por cada constructor de datos del AST*)
	val unitExp : unit -> exp
	val nilExp : unit -> exp
	val intExp : int -> exp
	val stringExp : string -> exp
	val callExp : TigerTemp.label * exp list * bool -> exp
	val opExp : TigerAbs.oper * exp * exp -> exp
	val recordExp : exp list -> exp
	val seqExp : exp list * exp * bool -> exp
	val assignExp : exp * exp -> exp
	val ifExp : exp * exp * exp * TigerAbs.ifop * bool -> exp
end
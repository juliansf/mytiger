signature TigerSemant =
sig
	type venv
	type tenv
	type expty
	val transVar : venv * tenv * tigerabs.var * tigerabs.pos -> expty
	val transExp : venv * tenv * tigerabs.exp -> expty
	val transDec : venv * tenv * tigerabs.dec -> { venv:venv, tenv:tenv }
	val transTy  : tenv * tigerabs.ty -> TigerTypes.ty
	val checkSemant : tigerabs.exp -> unit
end
signature TigerSemant =
sig
	type venv
	type tenv
	type expty
	val transVar : venv * tenv * TigerAbs.var * TigerAbs.pos -> expty
	val transExp : venv * tenv * TigerAbs.exp -> expty
	val transDec : venv * tenv * TigerAbs.dec -> { venv:venv, tenv:tenv }
	val transTy  : tenv * TigerAbs.ty -> TigerTypes.ty
	val checkSemant : TigerAbs.exp -> unit
end
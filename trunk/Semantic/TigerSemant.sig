signature TigerSemant =
sig
	type venv
	type tenv
	type expty
	val transVar : venv * tenv * TigerAbs.var * TigerAbs.pos * TigerTranslate.level -> expty
	val transExp : venv * tenv * TigerAbs.exp * TigerTranslate.level -> expty
	val transDec : venv * tenv * TigerAbs.dec * TigerTranslate.level * TigerTranslate.exp list -> { venv:venv, tenv:tenv, decs:TigerTranslate.exp list }
	val transTy  : tenv * TigerAbs.ty -> TigerTypes.ty
	val transProg : TigerAbs.exp -> TigerFrame.frag list
end
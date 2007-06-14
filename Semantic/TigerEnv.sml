structure TigerEnv =
struct
	open tigertab
	open TigerTypes
	
	datatype enventry =
		VarEntry of { access:TigerTranslate.access, ty:ty }
	| FunEntry of { level:TigerTranslate.level, label:TigerTemp.label, formals:ty list, result:ty }
	
	val tenv = tigertab.tabNueva()
	val venv = tigertab.tabNueva()
	
	val tenv = tabRInsert tenv ( "int", INT RW )
	val tenv = tabRInsert tenv ( "string", STRING )
	
	val outer = TigerTranslate.outermost
	val label = TigerTemp.namedlabel
	
	val venv = tabRInsert venv ( "print", FunEntry { level=outer, label=label("print"), formals=[STRING], result=UNIT } )
	val venv = tabRInsert venv ( "flush", FunEntry { level=outer, label=label("flush"), formals=[], result=UNIT } )
	val venv = tabRInsert venv ( "getchar", FunEntry { level=outer, label=label("getchar"), formals=[], result=STRING } )
	val venv = tabRInsert venv ( "ord", FunEntry { level=outer, label=label("ord"), formals=[STRING], result=INT RO } )
	val venv = tabRInsert venv ( "chr", FunEntry { level=outer, label=label("chr"), formals=[INT RW], result=STRING} )
	val venv = tabRInsert venv ( "size", FunEntry { level=outer, label=label("size"), formals=[STRING], result=INT RO } )
	val venv = tabRInsert venv ( "substring", FunEntry { level=outer, label=label("substring"), formals=[STRING, INT RW, INT RW], result=STRING } )
	val venv = tabRInsert venv ( "concat", FunEntry { level=outer, label=label("concat"), formals=[STRING, STRING], result=STRING } )
	val venv = tabRInsert venv ( "not", FunEntry { level=outer, label=label("not"), formals=[INT RW], result=INT RO } )
	val venv = tabRInsert venv ( "exit", FunEntry { level=outer, label=label("exit"), formals=[INT RW], result=UNIT } )
end
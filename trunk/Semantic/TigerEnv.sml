structure TigerEnv =
struct
	type access = unit
	type ty = TigerTypes.ty
	
	datatype enventry =
		VarEntry of { ty:ty }
	| FunEntry of { formals:ty list, result:ty }
end
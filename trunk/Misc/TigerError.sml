structure TigerError =
struct
	datatype error =
		ErrorInternalError of int
	| ErrorInvalidArgsTypesForOperator of tigerabs.oper
	| ErrorUndefinedFunction of string
	| ErrorWrongFunArgType of string * int
	| ErrorFewArgsForFunction of string
	| ErrorManyArgsForFunction of string
	| ErrorUndefindedType of string
	| ErrorReadOnlyVariable of tigerabs.var
	| ErrorAssignTypeMismatch of tigerabs.var
	| ErrorIfTest
	| ErrorIfTypeMismatch
	| ErrorWhileTest
	| ErrorWhileBody
	| ErrorForLowExp
	| ErrorForHiExp
	| ErrorForWrongBodyType
	| ErrorUndefinedType of string
	| ErrorArrayTypeMistmach
	| ErrorArraySizeTypeIsNotInt of TigerTypes.ty
	| ErrorUndefinedVariable of string
	| ErrorRecordFieldUndefined of tigerabs.var * string
	| ErrorVariableIsNotRecord of tigerabs.var
	| ErrorArrayIndexIsNotInt
	| ErrorVariableIsNotArray of tigerabs.var
	| ErrorVarDecInitIsUnit of string
	| ErrorVarDecTypeMismatch of string
	| ErrorFunDecTypeMismatch of string
	| ErrorEscapeVariableNotExists of string
	
	fun opName oper = 
		case oper of
			tigerabs.PlusOp => "'+'"
		|	tigerabs.MinusOp => "'-'"
		|	tigerabs.TimesOp => "'*'"
		|	tigerabs.DivideOp => "'/'"
		|	tigerabs.EqOp => "'='"
		|	tigerabs.NeqOp => "'<>'"
		|	tigerabs.LtOp => "'<'"
		|	tigerabs.LeOp => "'<='"
		|	tigerabs.GtOp => "'>'"
		|	tigerabs.GeOp => "'>='"
	
	fun Error ( err, pos ) = 
		raise Fail ( Int.toString( pos ) ^ ": error: " ^
		(case err of
			ErrorInternalError n => "internal error " ^ Int.toString( n )
		| ErrorInvalidArgsTypesForOperator oper => 
				"los argumentos del operador " ^ opName oper ^ " son invalidos."
		| ErrorUndefinedFunction func =>
				"la funcion \"" ^ func ^ "\" no esta definida."
		| ErrorWrongFunArgType (func, n) => 
				"tipo incompatible para el argumetno " ^ Int.toString( n ) ^ " de '" ^ func ^ "'"
		| ErrorFewArgsForFunction func => "Error 2."
		| ErrorManyArgsForFunction func => "Error 3."
		| ErrorUndefindedType typ => "Error 4."
		| ErrorReadOnlyVariable var => "Error 5."
		| ErrorAssignTypeMismatch var => "Error 6."
		| ErrorIfTest => "Error 7."
		| ErrorIfTypeMismatch => "Error 8."
		| ErrorWhileTest => "Error 9."
		| ErrorWhileBody => "Error 10."
		| ErrorForLowExp => "Error 11."
		| ErrorForHiExp => "Error 12."
		| ErrorForWrongBodyType => "Error 13."
		| ErrorUndefinedType typ => "Error 14."
		| ErrorArrayTypeMistmach => "Error 15."
		| ErrorArraySizeTypeIsNotInt ty => "Error 16."
		| ErrorUndefinedVariable var => "Error 17."
		| ErrorRecordFieldUndefined (var, field) => "Error 18."
		| ErrorVariableIsNotRecord record => "Error 19."
		| ErrorArrayIndexIsNotInt => "Error 20."
		| ErrorVariableIsNotArray var => "Error 21."
		| ErrorVarDecInitIsUnit var => "Error 22."
		| ErrorVarDecTypeMismatch var => "en la declaracion de la variable '" ^ var ^ "' no coinciden los tipos."
		| ErrorFunDecTypeMismatch func => "Error 24."
		| ErrorEscapeVariableNotExists var => "Error 25."))
end

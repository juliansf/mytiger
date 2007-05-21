structure TigerError =
struct
	open TigerProgName
	
	exception InternalError of string
	
	datatype errorfield =
		TypeMismatch of string
	| MissingField of string
	| DuplicatedField of string
	| FieldNotMember of string
	
	datatype error =
		ErrorInternalError of string
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
	| ErrorTypeIsNotRecord of string
	| ErrorArrayTypeMismatch of string
	| ErrorArraySizeTypeIsNotInt of TigerTypes.ty
	| ErrorUndefinedVariable of string
	| ErrorRecordFieldUndefined of tigerabs.var * string
	| ErrorRecordFields of errorfield list
	| ErrorVariableIsNotRecord of tigerabs.var
	| ErrorArrayIndexIsNotInt
	| ErrorVariableIsNotArray of tigerabs.var
	| ErrorVarDecInitIsUnit of string
	| ErrorVarDecTypeMismatch of string
	| ErrorFunDecTypeMismatch of string
	| ErrorEscapeVariableNotExists of string
	| ErrorTypeAlreadyDeclared of string	
	| ErrorRecursiveTypeDeclaration
	
	fun fieldError errty =
		case errty of
			TypeMismatch name => "tipo incompatible para el campo '" ^ name ^ "'.\n"
		| MissingField name => "falta el campo '" ^ name ^ "'.\n"
		| DuplicatedField name => "campo '" ^ name ^ "' duplicado.\n"
		| FieldNotMember name => "el campo '" ^ name ^ "' no es un miembro del record.\n"
	
	fun ErrorString err pos = 
	  !progName ^ ":" ^ Int.toString( pos ) ^ ": error: " ^
		(case err of
			ErrorInternalError errmsg => "internal error: " ^ errmsg
		| ErrorInvalidArgsTypesForOperator oper => 
				"los argumentos del operador " ^ tigerpp.stringFromOper oper ^ " son invalidos."
		| ErrorUndefinedFunction func =>
				"la funcion \"" ^ func ^ "\" no esta definida."
		| ErrorWrongFunArgType (func, n) => 
				"tipo incompatible para el argumento " ^ Int.toString( n ) ^ " de '" ^ func ^ "'"
		| ErrorFewArgsForFunction func => "Error 2."
		| ErrorManyArgsForFunction func => "Error 3."
		| ErrorUndefindedType typ => "Error 4."
		| ErrorReadOnlyVariable var => "Error 5."
		| ErrorAssignTypeMismatch var => "asignacion con tipos incompatibles."
		| ErrorIfTest => "Error 7."
		| ErrorIfTypeMismatch => "Error 8."
		| ErrorWhileTest => "Error 9."
		| ErrorWhileBody => "Error 10."
		| ErrorForLowExp => "Error 11."
		| ErrorForHiExp => "Error 12."
		| ErrorForWrongBodyType => "Error 13."
		| ErrorUndefinedType typ => "el tipo '" ^ typ ^ "' no esta definido."
		| ErrorTypeIsNotRecord typ => "el tipo '" ^ typ ^ "' no es un record."
		| ErrorArrayTypeMismatch typ => "tipo incompatible en la inicializacion del arreglo."
		| ErrorArraySizeTypeIsNotInt ty => "Error 16."
		| ErrorUndefinedVariable var => "variable indefinida '" ^ var ^ "'."
		| ErrorRecordFieldUndefined (var, field) => "Error 18."
		| ErrorRecordFields errList => (case errList of 
					[] => "" 
				| [x] => fieldError x 
				| (x::xs) => fieldError x ^ ErrorString (ErrorRecordFields xs) pos)
		| ErrorVariableIsNotRecord record => "Error 19."
		| ErrorArrayIndexIsNotInt => "Error 20."
		| ErrorVariableIsNotArray var => "la variable '" ^ tigerpp.stringFromVar var ^ "' no es un arreglo."
		| ErrorVarDecInitIsUnit var => "Error 22."
		| ErrorVarDecTypeMismatch var => "en la declaracion de la variable '" ^ var ^ "' no coinciden los tipos."
		| ErrorFunDecTypeMismatch func => "Error 24."
		| ErrorEscapeVariableNotExists var => "Error 25."
		| ErrorTypeAlreadyDeclared typ => "el tipo '" ^ typ ^ "' ya esta declarado en este batch."
		| ErrorRecursiveTypeDeclaration => "declaracion recursiva de tipos.")
		
	fun Error ( err, pos ) = raise Fail (ErrorString err pos ^ "\n")
end

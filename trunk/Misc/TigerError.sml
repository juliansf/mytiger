structure TigerError =
struct
	open TigerProgName
	
	exception InternalError of string
	exception UndefinedType of string
	exception DupFieldInRecordDec of string
	
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
	| ErrorDupFieldInRecordDec of string
	| ErrorRecordFields of errorfield list
	| ErrorVariableIsNotRecord of tigerabs.var
	| ErrorArrayIndexIsNotInt
	| ErrorVariableIsNotArray of tigerabs.var
	| ErrorVarDecInitIsUnit of string
	| ErrorVarDecTypeMismatch of string
	| ErrorFunDecTypeMismatch of string
	| ErrorFunAlreadyDeclared of string
	| ErrorDupFieldInFunDec of string * string
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
		| ErrorFewArgsForFunction func => "pocos argumentos pasados a la funcion '" ^ func ^ "."
		| ErrorManyArgsForFunction func => "demasiados argumentos pasados a la funcion '" ^ func ^ "'."
		| ErrorReadOnlyVariable var => "no se puede escribir una variable de solo lectura."
		| ErrorAssignTypeMismatch var => "asignacion con tipos incompatibles."
		| ErrorIfTest => "la condicion del 'if' debe ser un entero."
		| ErrorIfTypeMismatch => "tipos incompatibles en el 'if'."
		| ErrorWhileTest => "la condicion del 'while' debe ser un entero."
		| ErrorWhileBody => "el cuerpo del 'while' debe ser una sentencia."
		| ErrorForLowExp => "el inicializador del 'for' debe ser un entero."
		| ErrorForHiExp => "el finalizador del 'for' debe ser un entero." (*FINALIZADOR NO EXISTE! OJO! CAMBIAR ESTO*)
		| ErrorForWrongBodyType => "el cuerpo del 'for' debe ser una sentencia."
		| ErrorUndefinedType typ => "el tipo '" ^ typ ^ "' no esta definido."
		| ErrorTypeIsNotRecord typ => "el tipo '" ^ typ ^ "' no es un record."
		| ErrorArrayTypeMismatch typ => "tipo incompatible en la inicializacion del arreglo."
		| ErrorArraySizeTypeIsNotInt ty => "tipo incompatible en el tama0x23o del arreglo."
		| ErrorUndefinedVariable var => "variable indefinida '" ^ var ^ "'."
		| ErrorRecordFieldUndefined (var, field) => "'" ^ field ^ "' no es un miembro del record."
		| ErrorRecordFields errList => (case errList of 
					[] => "" 
				| [x] => fieldError x ^ "\b"
				| (x::xs) => fieldError x ^ ErrorString (ErrorRecordFields xs) pos)
		| ErrorDupFieldInRecordDec field => "el campo '" ^ field ^ "' esta repetido en la declaracion."
		| ErrorVariableIsNotRecord record => "la variable no es un record."
		| ErrorArrayIndexIsNotInt => "el indice del arreglo no es un entero."
		| ErrorVariableIsNotArray var => "la variable '" ^ tigerpp.stringFromVar var ^ "' no es un arreglo."
		| ErrorVarDecInitIsUnit var => "tipo incompatible en la asignacion."
		| ErrorVarDecTypeMismatch var => "en la declaracion de la variable '" ^ var ^ "' no coinciden los tipos."
		| ErrorFunDecTypeMismatch func => "tipos incompatibles en el retorno de la funcion '" ^ func ^ "'."
		| ErrorFunAlreadyDeclared func => "la funcion '" ^ func ^ "' ya esta declarada en este batch."
		| ErrorDupFieldInFunDec (func, param) => 
				"el parametro '" ^ param ^ "' esta repetido en la declaracion de la funcion '" ^ func ^ "'."
		| ErrorEscapeVariableNotExists var => "escapes: variable inexistente: '" ^ var ^ "'."
		| ErrorTypeAlreadyDeclared typ => "el tipo '" ^ typ ^ "' ya esta declarado en este batch."
		| ErrorRecursiveTypeDeclaration => "declaracion recursiva de tipos.")
		
	fun Error ( err, pos ) = raise Fail (ErrorString err pos ^ "\n")
end

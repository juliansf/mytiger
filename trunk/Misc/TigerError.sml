structure TigerError =
struct
	open TigerProgName
	
	exception InternalError of string
	exception UndefinedType of string
	exception DupFieldInRecordDec of string
	exception ErrorFileNotFound of string
	exception ErrorEmptyFile of string
	exception ErrorNoInputFiles of string
	
	datatype errorfield =
		TypeMismatch of string
	| MissingField of string
	| DuplicatedField of string
	| FieldNotMember of string
	
	datatype error =
		ErrorInternalError of string
	| ErrorInvalidArgsTypesForOperator of TigerAbs.oper
	| ErrorUndefinedFunction of string
	| ErrorWrongFunArgType of string * int
	| ErrorFewArgsForFunction of string
	| ErrorManyArgsForFunction of string
	| ErrorReadOnlyVariable of TigerAbs.var
	| ErrorAssignTypeMismatch of TigerAbs.var
	| ErrorIfTest of TigerAbs.ifop
	| ErrorIfTypeMismatch of TigerAbs.ifop
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
	| ErrorRecordFieldUndefined of TigerAbs.var * string
	| ErrorDupFieldInRecordDec of string
	| ErrorRecordFields of errorfield list
	| ErrorVariableIsNotRecord of TigerAbs.var
	| ErrorArrayIndexIsNotInt
	| ErrorVariableIsNotArray of TigerAbs.var
	| ErrorVarDecInitIsUnit of string
	| ErrorVarDecTypeMismatch of string
	| ErrorFunDecTypeMismatch of string
	| ErrorFunAlreadyDeclared of string
	| ErrorDupFieldInFunDec of string * string
	| ErrorEscapeVariableNotExists of string
	| ErrorTypeAlreadyDeclared of string
	| ErrorRecursiveTypeDeclaration
	
	(* Errores de Parsing *)
	| ErrorParsingError of string
	| ErrorUnterminatedComment
	| ErrorEOFUnterminatedString
	| ErrorNLUnterminatedString
	| ErrorIllegalCharInString of string
	| ErrorInvalidCharBetweenBars of string
	| ErrorInvalidTypeName
	
	fun ErrorUnrecognizedOption name opt = print (name ^ ": opcion no reconocida '" ^ opt ^ "'\n")
	
	fun stringFromOper oper = 
		case oper of
			TigerAbs.Plus => "+"
		|	TigerAbs.Minus => "-"
		|	TigerAbs.Times => "*"
		|	TigerAbs.Div => "/"
		|	TigerAbs.Eq => "="
		|	TigerAbs.Neq => "<>"
		|	TigerAbs.Lt => "<"
		|	TigerAbs.Leq => "<="
		|	TigerAbs.Gt => ">"
		|	TigerAbs.Geq => ">="

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
				"los argumentos del operador " ^ stringFromOper oper ^ " son incompatibles."
		| ErrorUndefinedFunction func =>
				"la funcion \"" ^ func ^ "\" no esta definida."
		| ErrorWrongFunArgType (func, n) => 
				"tipo incompatible para el argumento " ^ Int.toString( n ) ^ " de '" ^ func ^ "'"
		| ErrorFewArgsForFunction func => "pocos argumentos pasados a la funcion '" ^ func ^ "."
		| ErrorManyArgsForFunction func => "demasiados argumentos pasados a la funcion '" ^ func ^ "'."
		| ErrorReadOnlyVariable var => "no se puede escribir una variable de solo lectura."
		| ErrorAssignTypeMismatch var => "asignacion con tipos incompatibles."
		| ErrorIfTest oper => 
				(case oper of 
					TigerAbs.If => "la condicion del 'if' debe ser un entero."
				|	TigerAbs.And => "el primer operando de '&' no es un entero."
				| TigerAbs.Or => "el primer operando de '|' no es un entero.")
		| ErrorIfTypeMismatch oper => 
				(case oper of
					TigerAbs.If => "tipos incompatibles en el 'if'."
				| TigerAbs.And => "el segundo operando de '&' no es un entero."
				| TigerAbs.Or => "el segundo operando de '|' no es un entero.")
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
		| ErrorVariableIsNotArray var => "la variable no es un arreglo."
		| ErrorVarDecInitIsUnit var => "tipo incompatible en la asignacion."
		| ErrorVarDecTypeMismatch var => "en la declaracion de la variable '" ^ var ^ "' no coinciden los tipos."
		| ErrorFunDecTypeMismatch func => "tipos incompatibles en el retorno de la funcion '" ^ func ^ "'."
		| ErrorFunAlreadyDeclared func => "la funcion '" ^ func ^ "' ya esta declarada en este batch."
		| ErrorDupFieldInFunDec (func, param) => 
				"el parametro '" ^ param ^ "' esta repetido en la declaracion de la funcion '" ^ func ^ "'."
		| ErrorEscapeVariableNotExists var => "escapes: variable inexistente: '" ^ var ^ "'."
		| ErrorTypeAlreadyDeclared typ => "el tipo '" ^ typ ^ "' ya esta declarado en este batch."
		| ErrorRecursiveTypeDeclaration => "declaracion recursiva de tipos."
		
		(* Errores de Parsing *)
		| ErrorParsingError str => "error de parseo causado por '" ^ str ^ "'."
		| ErrorUnterminatedComment => "fin de archivo alcanzado con un comentario sin cerrar."
		| ErrorEOFUnterminatedString => "fin de archivo alcanzado con una cadena no cerrada."
		| ErrorNLUnterminatedString => "fin de linea encontrado antes de cerrar la cadena."
		| ErrorIllegalCharInString str => "cadena malformada con caracter invalido (" ^ str ^ ")."
		| ErrorInvalidCharBetweenBars str => "caracter invalido entre las barras (" ^ str ^ ")."
		| ErrorInvalidTypeName => "nombre de tipo invalido en la inicializacion del arreglo.")
	
		
	fun Error ( err, pos ) = raise Fail (ErrorString err pos ^ "\n")
	
	fun ShowErrors e =
		case e of
			Fail s => print(s)
		|	ErrorFileNotFound name => print ( name ^ ": archivo inexistente.\n" )
		| ErrorEmptyFile name => print ( name ^ ": archivo vacio.\n" )
		| ErrorNoInputFiles name => print ( name ^ ": no hay archivos fuentes que compilar.\n" )
		| e => raise e
end

structure TigerSemant :> TIGERSEMANT =
struct
	open TigerTab
	open TigerEnv
	open TigerTree
	open TigerTypes
	
	type venv = (String, enventry) tabla 
	type tenv = (String, ty) tabla
	type exty = { exp: tree, ty:ty }
	
	
	(* Despues vemos donde va esta funcion *)
	fun weakCompTypes ( a, b ) = 
		case ( a, b ) of
			( NIL, NIL ) => false (* VER ESTO!!! *)
		| ( UNIT, UNIT ) => true
		|	( INT _, INT _ ) => true
		| ( STRING, STRING ) => true
		| ( RECORD _, NIL ) => true
		| ( NIL, RECORD _ ) => true
		| ( RECORD (_,uniq1), RECORD (_,uniq2) ) => uniq1 = uniq2
		| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => uniq1 = uniq2
		| _ => Error ( ErrorAUCH, pos)(* Completar. Faltaria para NAME que no se como se hace *)	
			
	
	
	(******************************************************)
	(* TransExp:                                          *)
	(*  Tipa las expresiones y devuelve codigo intermedio *)
	(******************************************************)
	fun transExp ( venv, tenv, expr ) =
		let 
			fun trexp ( VarExp (v,p) ) = transVar ( venv, tenv, v, p )
				| trexp ( UnitExp _ ) = { exp=NN, ty=UNIT }
				| trexp ( NilExp _ ) = { exp=NN, ty=NIL }
				| trexp ( IntExp (n,p) ) = { exp=NN, ty=INT RO } (* Ojo! El RO puede no estar bien *)
				| trexp ( StringExp (s,p) ) = { exp=NN, ty=STRING }
				| trexp ( CallExp ({ func, args }, pos) ) =
						let 
							val { formals, result } =
							(* Buscamos la funcion en el entorno *)
							case tabSearch venv func of
								(* Obtenemos los parametros formales y el tipo del resultado *)
								FunEntry { formals, result } => { formals=formals, result=result }
							| _ => Error ( ErrorUndefinedFunction func, pos )
							
							(* Obtenemos los tipos de los argumentos pasados a la funcion *)
							val tyList = List.map ((fn r => #ty(r)) o trexp) args 
								(* <<< Si despues se necesitan las exp, hay que cambiar lo de arriba >>> *)
							
							(* 
								Usamos equalTypes para chequear cada uno de los tipos de los argumentos 
								pasados a la funcion. Si alguno esta mal, se informa cual es la
								posicion del argumento.
							*)
							val count = ref 1
							fun equalTypes (a, b) = 
								if weakCompTypes ( a, b ) then ( count := !count + 1; true )
								else Error ( ErrorWrongFunArgType count, pos )
						in
							(* La cantidad de argumentos pasados es la correcta *)
							if List.length tyList = List.length formals
							then 
								( 
									(* Se chequean los parametros *)
									ListPair.all equalTypes tyList formals; 
									{ exp=NN, ty=result } 
								)
							else
								(* La cantidad de argumetnos pasados es menor o mayor a la esperada *)
								if List.length tyList < List.length formals
								then Error ( ErrorFewArgsForFunction func, pos )
								else Error ( ErrorManyArgsForFunction func, pos )
						end
				| trexp ( OpExp ({left, oper, right}, pos) ) = (* Operadores Binarios *)
						let 
							(* Aplicamos trexp a los operandos *)
							val { exp=explleft, ty=tyleft } = trexp left
							val { exp=expright, ty=tyright } = trexp right
							
							fun i () = (* Int *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=NN, ty=INT }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
							
							fun is () = (* Int, String *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=NN, ty=INT }
								| ( STRING, STRING ) => { exp=NN, ty=INT }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								
							fun isar () = (* Int, String, Array, Record *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=NN, ty=INT }
								| ( STRING, STRING ) => { exp=NN, ty=INT }
								| ( RECORD (_,uniq1), RECORD (_,uniq2) ) =>
										if uniq1 = uniq2 then { exp=NN, ty=INT }
										else Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								| ( RECORD _, NIL ) => { exp=NN, ty=INT }
								| ( NIL, RECORD _ ) => { exp=NN, ty=INT }
								| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => 
										if uniq1 = uniq2 then { exp=NN, ty=INT }
										else Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
						in
							case oper of
								PlusOp 	=> i()
							| MinusOp => i()
							| TimesOp => i()
							| DivideOp 	=> i()
							| EqOp 		=> isar()
							| NeqOp 	=> isar()
							| GtOp		=> is()
							| GeOp		=> is()
							| LtOp		=> is()
							| LeOp 	=> is()
						end
				| trexp ( RecordExp ({ fields, typ }, pos) ) = 
						let val ty = tabSearch tenv typ
						in
							case ty of
								RECORD x => { exp=NN, ty=RECORD x }
							|	_ => Error ( ErrorUndefindedType typ, pos)
						end
				| trexp ( SeqExp ([], pos) ) = { exp=NN, ty=UNIT }
				| trexp (	SeqExp (exp :: [], pos) ) = trexp exp
				| trexp ( SeqExp (exp :: expl) ) = ( trexp exp; trexp (SeqExp (expl, pos)) )	
						
				| trexp ( AssignExp ({ var, exp }, pos) ) =
						let
							val { exp=expvar, ty=tyvar } = trexp var
							val { exp=expexp, ty=tyexp } = trexp exp
						in
							if tyvar = INT RO then Error ( ErrorReadOnlyVariable var, pos )
							else 
								if tyvar = tyexp then { exp=NN, ty=UNIT }
								else Error ( ErrorAssignTypeMismatch var, pos )
						end
				| trexp ( IfExp ({ test, then', else' },pos) ) =
						let
							val { exp=exptest, ty=tytest } = trexp test
							val { exp=expthen, ty=tythen } = trexp then'
							val { exp=expelse, ty=tyelse } =
								case else' of
									SOME else'' => trexp else''
							  | NONE => { exp=NN, ty=UNIT }
						in
							if not (isInt tytest) then Error ( ErrorIfTest, pos ) 
							else
								if weakCompTypes (tythen, tyelse) then { exp=NN, ty=tythen }
								else Error ( ErrorIfTypeMismatch, pos )
						end
				| trexp ( WhileExp ({ test, body }, pos) ) =
						let 
							val { exp=exptest, ty=tytest } = trexp test
							val { exp=expbody, ty=tybody } = trexp body
						in
							if not (isInt tytest) then Error ( ErrorWhileTest, pos ) 
							else
								if tybody = UNIT then { exp=NN, ty=UNIT }
								else Error ( ErrorWhileBody, pos )
						end
				| trexp ( ForExp ({ var, escape, lo, hi, body }, pos) ) =
						let
							val { exp=explo, ty=tylo } = trexp lo
							val { exp=exphi, ty=tyhi } = trexp hi
							
							val venv' = fromTable venv
							val venv' = tabRInsert venv' (var, VarEntry { ty=INT RO } )
							val { exp=expbody, ty=tybody } = transExp ( venv', tenv, body )
						in 
							if not (isInt tylo) then Error ( ErrorForLowExp lo, pos ) else ();
							if not (isInt tyhi) then Error ( ErrorForHiExp hi, pos ) else ();
							if tbody <> UNIT then Error ( ErrorForWrongBodyType, pos ) else ();
							{ exp=NN, ty=UNIT }
						end
				| trexp ( LetExp ({ decs, body }, pos) ) =
						let
							val { venv=venv', tenv=tenv'} =
								List.foldl (fn (x,y) => transDec (#venv(y), #tenv(y), x)) 
													 {venv=venv, tenv=tenv} decs
								val { exp=expbody, ty=tybody } = transExp ( venv', tenv', body )
						in
							{ exp=NN, ty=tybody }
						end
				| trexp ( BreakExp pos ) = { exp=NN, ty=UNIT }
				| trexp ( ArrayExp ({typ, size, init}, pos) ) =
						let
							val aty = case tabSearch tenv typ of
								SOME (ATY as ARRAY (t,_)) => t
								| NONE => Error ( ErrorUndefinedType typ, pos )
							val {exp=expsize, ty=tysize} = trexp size
							val {exp=expinit, ty=tyinit} = trexp init
						in
							if not (isInt(tySize)) then 
								Error ( ErrorArrayTypeMistmach aty tyinit, pos ) else ();
							if not (weakCompTypes(aty, tyinit)) then  
								Error ( ErrorArraySizeTypeIsNotInt tysize, pos ) else ();
							{exp=NN, ty=ATY}
						end						
		in
			trexp expr
		end

	and transVar ( venv, tenv, SimpleVar v, pos ) =
		let
			val ty = case tabSearch venv v of
				SOME (VarEntry { ty }) => ty
			| NONE => Error ( ErrorUndefinedVariable v, pos )
		in
			{ exp=NN, ty=ty }
		end

	| transVar ( venv, tenv, FieldVar (var, symbol), pos) =
		let
			val { exp=expvar, ty=tyvar } = transVar ( venv, tenv, var, pos )
		in
			case tyvar of
				RECORD (ml, _) => (
					case List.find (fn (x,y) => x = symbol) ml of
						SOME (sym,ty) => { exp=NN, ty=ty }
					| NONE => Error ( ErrorRecordFieldUndefined sym, pos ) )
			| _ => Error ( ErrorVariableIsNotRecord var, pos )
		end
			
	| transVar ( venv, tenv, SubscriptVar (var, exp), pos ) =
		let
			val { exp=expvar, ty=tyvar } = transVar ( venv, tenv, var, pos )
			val { exp=expexp, ty=tyexp } = transExp ( venv, tenv, exp )
		in
			case tyvar of
				ARRAY (ty, _) => 
					if isInt tyexp then { exp=NN, ty=ty }
					else Error ( ErrorArrayIndexIsNotInt ty, pos )
			| _ => Error ( ErrorVariableIsNotArray var, pos )
		end
	
	and transDec ( venv, tenv, VarDec ({ name, escape, typ=NONE, init}, pos ) ) =
		let
			val { exp, ty } = transExp ( venv, tenv, init )
			val venv = tabRInsert venv ( name, VarEntry { ty=ty } )
		in
			if tyinit <> UNIT then { venv=venv, tenv=tenv }
			else Error ( ErrorVarDecInitIsUnit, pos )
		end
	
	| transDec ( venv, tenv, VarDec ({ name, escape, typ=SOME ty, init }, pos ) ) =
		let
			val ty' = case tabSearch tenv ty of
				SOME t => t
			| NONE => Error ( ErrorUndefinedType ty, pos )
			
			val { exp=expinit, ty=tyinit } = transExp ( venv, tenv, init )
		in
			if weakCompTypes ( ty', tyinit ) 
			then { venv=tabRInsert venv ( name, VarEntry { ty=ty } ) }
			else Error ( ErrorVarDecTypeMismatch ty tyinit, pos )
	  end
	
	| transDec ( venv, tenv, lfuncs ) =
		let 
			fun trdec1 ({name, params, result, body}, pos) =
				let
					val formals = List.map (fn x => #typ(x)) params
					val result = case result of
						SOME res => (
							case tabSearch tenv res of
								SOME t => t
							|	NONE => Error ( ErrorUndefinedType res, pos ) )
					| NONE => UNIT
				in
					tabRInsert venv ( name, FunEntry { formals=formals, result=result } );
					()
				end
				
			fun trdec2 ({name, params, result, body}, pos)=
				let
					val formals = List.map (fn x => {name= })
					val venv' = fromTable venv
					val venv' = List.app () params 
				in
				
				()
				end
		in
			List.app trdec1 lfuncs;
			List.app trdec2 lfuncs
		end
		
		
	and transTy (tvenv, ty, pos) =
		case tabSearch tenv ty of
				SOME t => t
			| NONE => Error ( ErrorUndefinedType ty, pos )
		
end
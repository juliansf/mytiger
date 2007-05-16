structure TigerSemant :> TigerSemant =
struct
	open tigerabs
	open tigertab
	open TigerEnv
	open TigerTrace
	open TigerTypes
	open TigerUtils
	open TigerError
	
	type venv = (string, enventry) Tabla 
	type tenv = (string, ty) Tabla
	type expty = { exp: TigerTrace.tree, ty:ty }
	
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
								SOME (FunEntry { formals, result }) => { formals=formals, result=result }
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
								else Error ( ErrorWrongFunArgType (func, !count), pos )
						in
							(* La cantidad de argumentos pasados es la correcta *)
							if List.length tyList = List.length formals
							then 
								( 
									(* Se chequean los parametros *)
									ListPair.all equalTypes (tyList, formals); 
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
									( INT _, INT _) => { exp=NN, ty=INT RO }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
							
							fun is () = (* Int, String *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=NN, ty=INT RO }
								| ( STRING, STRING ) => { exp=NN, ty=INT RO }
								| _ => Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								
							fun isar () = (* Int, String, Array, Record *)
								case ( tyleft, tyright ) of
									( INT _, INT _) => { exp=NN, ty=INT RO }
								| ( STRING, STRING ) => { exp=NN, ty=INT RO }
								| ( RECORD (_,uniq1), RECORD (_,uniq2) ) =>
										if uniq1 = uniq2 then { exp=NN, ty=INT RO }
										else Error ( ErrorInvalidArgsTypesForOperator oper, pos )
								| ( RECORD _, NIL ) => { exp=NN, ty=INT RO }
								| ( NIL, RECORD _ ) => { exp=NN, ty=INT RO }
								| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => 
										if uniq1 = uniq2 then { exp=NN, ty=INT RO }
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
						let
							val (rty, realfields) = case tabSearch tenv typ of
								SOME (R as RECORD(fl,_)) => (R, fl)
								| SOME _ => Error (ErrorTypeIsNotRecord typ, pos)
								| _ => Error (ErrorUndefinedType typ, pos)
							val errorList = ref []
							val expList = ref []
							fun comp x (y,z) = x = y
							fun checkField defl (name, ty) =
								case List.find (comp name) (!defl) of
									SOME (_, e) => let
															val {exp=expexp, ty=tyexp} = trexp e 
														in
															defl := listRemoveItem (comp name) (!defl);
															if weakCompTypes (tyexp, ty) then
																expList := (name, expexp) :: (!expList)
															else
																errorList := !errorList @ [TypeMismatch name]
														end
									| NONE => errorList := !errorList @ [MissingField name]
							fun checkError (name, _) =
								case List.find (comp name) realfields of
									SOME _ => DuplicatedField name
									| NONE => FieldNotMember name
							val defl = ref fields;
						in
							List.app (checkField defl) realfields;
							case !errorList @ List.map checkError (!defl) of
								[] => {exp=NN, ty=rty}
								| l => Error (ErrorRecordFields l, pos)
						end

				| trexp ( SeqExp ([], pos) ) = { exp=NN, ty=UNIT }
				| trexp (	SeqExp (exp :: [], pos) ) = trexp exp
				| trexp ( SeqExp (exp :: expl, pos) ) = ( trexp exp; trexp (SeqExp (expl, pos)) )	
						
				| trexp ( AssignExp ({ var, exp }, pos) ) =
						let
							val { exp=expvar, ty=tyvar } = transVar (venv, tenv, var, pos)
							val { exp=expexp, ty=tyexp } = trexp exp
						in
							if tyvar = INT RO then Error ( ErrorReadOnlyVariable var, pos )
							else 
								if weakCompTypes (tyvar, tyexp) then { exp=NN, ty=UNIT }
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
							if not (isInt tylo) then Error ( ErrorForLowExp, pos ) else ();
							if not (isInt tyhi) then Error ( ErrorForHiExp, pos ) else ();
							if tybody <> UNIT then Error ( ErrorForWrongBodyType, pos ) else ();
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
							val (aty, auniq) = case tabSearch tenv typ of
								SOME (ARRAY (t,u)) => (t,u)
								| _ => Error ( ErrorUndefinedType typ, pos )
							val { exp=expsize, ty=tysize } = trexp size
							val { exp=expinit, ty=tyinit } = trexp init
						in
							if not (isInt(tysize)) then 
								Error ( ErrorArraySizeTypeIsNotInt tysize, pos ) else ();
							if not (weakCompTypes(aty, tyinit)) then  
                                Error ( ErrorArrayTypeMistmach, pos ) else ();							

							{ exp=NN, ty=ARRAY (aty, auniq) }
						end						
		in
			trexp expr
		end

	and transVar ( venv, tenv, SimpleVar v, pos ) =
		let
			val ty = case tabSearch venv v of
				SOME (VarEntry { ty }) => ty
			| _ => Error ( ErrorUndefinedVariable v, pos )
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
					| NONE => Error ( ErrorRecordFieldUndefined (var, symbol), pos ) )
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
					else Error ( ErrorArrayIndexIsNotInt, pos )
			| _ => Error ( ErrorVariableIsNotArray var, pos )
		end
	
	and transDec ( venv, tenv, VarDec ({ name, escape, typ=NONE, init}, pos ) ) =
		let
			val { exp=expinit, ty=tyinit } = transExp ( venv, tenv, init )
			val tyinit = case tyinit of INT _ => INT RW | t => t
			val venv = tabRInsert venv ( name, VarEntry { ty=tyinit } )
		in
			if tyinit <> UNIT then { venv=venv, tenv=tenv }
			else Error ( ErrorVarDecInitIsUnit name, pos )
		end
	
	| transDec ( venv, tenv, VarDec ({ name, escape, typ=SOME ty, init }, pos ) ) =
		let
			val ty' = case tabSearch tenv ty of
				SOME t => t
			| NONE => Error ( ErrorUndefinedType ty, pos )
			
			val { exp=expinit, ty=tyinit } = transExp ( venv, tenv, init )
		in
			if weakCompTypes ( ty', tyinit ) 
			then { venv=tabRInsert venv ( name, VarEntry { ty=tyinit } ), tenv=tenv }
			else Error ( ErrorVarDecTypeMismatch name, pos )
	  end
	
	| transDec ( venv, tenv, FunctionDec lfuncs ) =
		let 
			fun trdec1 ({name, params, result, body}, pos) =
				let
					fun chkType { name, escape, typ } =
						case tabSearch tenv typ of
							SOME t => t
						| NONE => Error ( ErrorUndefinedType typ, pos )
					val formals = List.map chkType params
					
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
					fun insertField env { name, escape, typ } =
						case tabSearch tenv typ of
							SOME ty => ( tabRInsert env (name, VarEntry{ ty=ty }); () )
						| NONE => Error ( ErrorInternalError 1, pos )
						
					val venv' = fromTable venv
					val _ = List.app (insertField venv') params
					
					val { exp=expbody, ty=tybody } = transExp (venv', tenv, body)
					
					val tyres = case tabSearch venv name of
											 	SOME (FunEntry { formals, result }) => result
											| _ => Error ( ErrorInternalError 2, pos )
				in
					if weakCompTypes (tyres, tybody) then ()
					else Error ( ErrorFunDecTypeMismatch name, pos )
				end
		in
			List.app trdec1 lfuncs;
			List.app trdec2 lfuncs;
			{ venv=venv, tenv=tenv }
		end
		
	| transDec ( venv, tenv, TypeDec ltdecs ) = { venv=venv, tenv=tenv } (* Falta completar *)
		
	and transTy (tenv, NameTy ty) =
		(case tabSearch tenv ty of
				SOME t => t
			| NONE => NAME ( ty, ref NONE ))
		
	| transTy (tenv, RecordTy flist) =
			let
				fun aux { name, escape, typ } = ( name, transTy (tenv, NameTy typ))
			in
				RECORD (List.map aux flist, ref ()) 
			end
	
	| transTy (tenv, ArrayTy ty) = ARRAY ( transTy (tenv, NameTy ty), ref ())
	
	fun checkSemant prog = 
		let
			val tenv = tigertab.tabNueva()
			val venv = tigertab.tabNueva()
			
			val tenv = tabRInsert tenv ( "int", INT RW )
			val tenv = tabRInsert tenv ( "string", STRING )
			val tenv = tabRInsert tenv ("r", RECORD ( [("a", INT RW), ("b", STRING), ("c", INT RW)],ref ()))
		in
			transExp ( venv, tenv, prog);
			()
		end
		
end

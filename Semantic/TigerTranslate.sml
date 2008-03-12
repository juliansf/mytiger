structure TigerTranslate :> TigerTranslate =
struct
	open TigerTemp
	open TigerError
	open TigerAbs
	open TigerTree
	open TigerFrame
	
	type frag = TigerFrame.frag
	
	type level = { depth: int, lstack: label list ref, frame: frame, parent: frame option }
	type access = level * TigerFrame.access
	
	val outermost = {depth=0, lstack = ref [], frame = newFrame (namedlabel "_tigermain", []), parent=NONE }
	
	val lfrag : frag list ref = ref []
							
	fun getResult() = !lfrag
	fun addProc(body, frame) = lfrag := PROC {body=body, frame=frame} :: (!lfrag) 
	fun addString(label, string) = lfrag := STRING (label, string) :: (!lfrag)
	
	fun toplb lst = hd(!lst)
	fun pushlb lst x = lst := x :: (!lst)
	fun poplb lst = lst := tl(!lst)
	
	fun preWhileFor (level:level) = pushlb (#lstack(level)) (newlabel())
	fun posWhileFor (level:level) = poplb (#lstack(level))

	fun newLevel (parent:level, name, formals) = {depth = #depth(parent) + 1,
												  lstack = ref [],
												  frame = newFrame (namedlabel2 name, formals),
												  parent = SOME (#frame(parent)) }
		
	fun getLevelLabel (level:level) = getFrameLabel(#frame(level))
	
	fun formals (level:level) = tl (List.map (fn x => (level, x)) (TigerFrame.formals (#frame(level))))
	
	fun allocLocal (level:level) escapes =
		(level, TigerFrame.allocLocal (#frame(level)) escapes)

	datatype exp =
	  Ex of TigerTree.exp (* Expresion *)
	| Nx of TigerTree.stm (* No Result *)
	| Cx of TigerTemp.label * TigerTemp.label -> TigerTree.stm (* Condicion *)

	(* Funciones desempaquetadoras de expresiones *)
	fun unEx (Ex e) = e
      | unEx (Nx s) = ESEQ (s, CONST 0)
	  | unEx (Cx c) = 
			let 
				val (lv, lf) = (newlabel(), newlabel())
				val temp = newtemp()
			in
				ESEQ ( seq [ MOVE ( TEMP temp, CONST 0 ),
							 c (lv, lf),
							 LABEL lv,
							 MOVE ( TEMP temp, CONST 1 ),
							 LABEL lf ],
					   TEMP temp )
			end
			
	fun unNx (Ex e) = EXP e
	  | unNx (Nx s) = s
	  | unNx (Cx c) = 
			let 
				val (lv, lf) = (newlabel(), newlabel())
			in
				seq [c (lv, lf), LABEL lv, LABEL lf]
			end
	
	fun unCx (Ex (CONST n)) = (fn (lv, lf) => JUMP (NAME (if n = 0 then lf else lv), [lv, lf]))
      | unCx (Ex e) = (fn (lv, lf) => CJUMP ( NE, e, CONST 0, lv, lf ))
	  | unCx (Nx s) = Error ( ErrorInternalError "problemas con Translate.unCx!", 0)
	  | unCx (Cx c) = c
		
	(* Funciones de traduccion a codigo intemedio *)
	fun unitExp () = Ex (CONST 0)
	fun nilExp () = Ex (CONST 0) (* puntero a null *)
	fun intExp n = Ex (CONST n)
	
	fun stringExp s = 
		let
			val l = newlabel()
		in
			addString(l,s);
			Ex (NAME l)
		end
		
	fun callExp (name, params, caller:level, callee:level, proc:bool) =
		let
			(* sl_access computa el static link a pasar como 1er argumento en un CALL *)
			fun sl_access ~1 = MEM (BINOP (PLUS, CONST stackBias, TEMP FP))
	  		  | sl_access  0 = MEM (BINOP (PLUS, CONST (slOffset + stackBias), TEMP FP))
  			  | sl_access  n = MEM (BINOP (PLUS, CONST (slOffset), sl_access (n-1)))
  			  
			(* Calculamos el static link y lo agregamos al comienzo de la lista de argumentos formales *)
			val sl = sl_access ( #depth(caller) - #depth(callee) )
			val params' = sl :: List.map unEx params
		in
			if not proc then
				let
					val temp = newtemp()
				in
					Ex (ESEQ (SEQ (EXP (CALL (NAME name, params')), MOVE (TEMP temp, TEMP O0)), TEMP temp))
				end
			else
				Nx (EXP (CALL (NAME name, params')))
		end
	
	fun opExp (oper, exp1, exp2) =
		let 
			val exp1 = unEx exp1 and exp2 = unEx exp2
		in
			case oper of
				Plus => Ex (BINOP (PLUS, exp1, exp2))
			| Minus => Ex (BINOP (MINUS, exp1, exp2))
			| Times => Ex (BINOP (MUL, exp1, exp2))
			| Div => Ex (BINOP (DIV, exp1, exp2))
			| Eq => Cx (fn (lv,lf) => CJUMP (EQ, exp1, exp2, lv, lf))
			| Neq => Cx (fn (lv,lf) => CJUMP (NE, exp1, exp2, lv, lf))
			| Gt => Cx (fn (lv,lf) => CJUMP (GT, exp1, exp2, lv, lf))
			| Geq => Cx (fn (lv,lf) => CJUMP (GE, exp1, exp2, lv, lf))
			| Lt => Cx (fn (lv,lf) => CJUMP (LT, exp1, exp2, lv, lf))
			| Leq => Cx (fn (lv,lf) => CJUMP (LE, exp1, exp2, lv, lf))
		end
	
	fun recordExp inits =
		let 
			val inits' = List.map unEx inits
			val temp = newtemp()
		in
			Ex (ESEQ (
						SEQ (EXP (externalCall ("_createRecord", (CONST (List.length inits)::inits'))),
								MOVE (TEMP temp, TEMP O0)), 
						TEMP temp))
		end
	
	fun seqExp (expl, lastexp, proc) = 
		if proc then Nx (SEQ (seq (List.map unNx expl), unNx lastexp))
		else Ex (ESEQ (seq (List.map unNx expl), unEx lastexp))
	
	fun assignExp (var, exp) = Nx (MOVE (unEx var, unEx exp))
	
	fun ifExp (a, b, _, TigerAbs.And, _) =
		Cx (fn (lv, lf) => let val li = newlabel()
											 in seq([(unCx a) (li,lf), LABEL li, (unCx b) (lv,lf)]) end)

	| ifExp (a, _, b, TigerAbs.Or, _) =
		Cx (fn (lv, lf) => let val li = newlabel()
											 in seq([(unCx a) (lv,li), LABEL li, (unCx b) (lv,lf)]) end)	
		
	| ifExp (cond, then', else', TigerAbs.If, false) =
		let
			val (lv, lf, join) = (newlabel(), newlabel(), newlabel())
			val temp = newtemp()
		in
			Ex (ESEQ (
						seq [(unCx cond) (lv, lf),
								LABEL lv, MOVE (TEMP temp, unEx then'), JUMP (NAME join, [join]),
								LABEL lf, MOVE (TEMP temp, unEx else'), LABEL join],
						TEMP temp))
		end
	
	| ifExp (cond, then', else', TigerAbs.If, true) =
		let
			val (lv, lf, join) = (newlabel(), newlabel(), newlabel())
		in
			Nx (seq [(unCx cond) (lv, lf),
								LABEL lv, unNx then', JUMP (NAME join, [join]),
								LABEL lf, unNx else', LABEL join])
		end
		
	fun whileExp (test, body, level:level) =
		let
			val (lv, lf, ls) = (newlabel(), toplb(#lstack(level)), newlabel())
		in
			Nx(seq[LABEL ls, (unCx test) (lv, lf), LABEL lv, unNx body, JUMP (NAME ls, [ls]), LABEL lf]) 
		end
		
	fun forExp (var, lo, hi, body, level:level) =
		let
			val index' = unEx var
			val (loop, sale, sigue) = (newlabel(), toplb(#lstack(level)), newlabel())
			val tmphi = newtemp() 
		in
			Nx (seq [ MOVE (index', unEx lo),
				MOVE (TEMP tmphi, unEx hi),
				CJUMP (GT, index', TEMP tmphi, sale, loop),
				LABEL sigue,
				MOVE (index', BINOP (PLUS, index', CONST 1)),
				LABEL loop,
				unNx body,
				CJUMP (GE, index', TEMP tmphi, sale, sigue),
				LABEL sale])
		end
	
	fun letExp (inits, body, true) = Nx (seq(List.map unNx inits @ [unNx body]))
		| letExp (inits, body, false) = Ex(
				case inits of 
					[] => unEx body 
				| inits => ESEQ (seq (List.map unNx inits), unEx body))

	fun breakExp (level:level) =
		let 
			val label = toplb (#lstack(level))
				handle Empty => raise BreakError
		in
			Nx(JUMP (NAME label, [label]))
		end
		
	fun arrayExp(size, init)=
		let 
			val temp = newtemp()
		in
			Ex (ESEQ (
						SEQ (EXP (externalCall ("_createArray", [unEx size, unEx init]) ),
								MOVE (TEMP temp, TEMP O0)), 
						TEMP temp))
		end		
	
	(* Traduccion de Variables *)
	fun simpleVar ((varlevel:level, access), actuallevel:level) = 
		let 
			(* aux genera el código que recorre los static links hasta 
			   llegar al frame en el nivel correspondiente *)
			fun aux 0 = MEM (BINOP (PLUS, CONST stackBias, TEMP FP))
			  | aux n = MEM (BINOP (PLUS, CONST slOffset, aux (n-1)))
		in
			Ex (exp access (aux (#depth(actuallevel) - #depth(varlevel))))
		end
	
	fun fieldVar (varaddr, offset) =
			Ex ( MEM (BINOP (PLUS, BINOP (MUL, CONST wordSize, CONST offset), unEx varaddr)))
	
	fun subscriptVar (varaddr, expOffset) =
			Ex ( MEM (BINOP (PLUS, BINOP (MUL, CONST wordSize, unEx expOffset), unEx varaddr)))
	
	(* Traduccion de declaraciones de variables*)
	fun varDec ( ( _ , access), init) = 
		let
			val varaddr = exp access (TEMP FP)
		in
			Nx (MOVE (varaddr, unEx init))
		end
		
	(* Construccion de un nuevo Fragmento de tipo PROC *)
	fun procEntryExit (level:level, body) =
		let
			val body' = case body of 
			               Nx s => s
			             | s => MOVE (TEMP I0, unEx s)
									
			val frame = #frame(level)
		in
			addProc(procEntryExit1 (body', frame), frame )
		end	
end

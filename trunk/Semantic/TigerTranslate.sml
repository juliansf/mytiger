structure TigerTranslate :> TigerTranslate =
struct
	open TigerTemp
	open TigerError
	open TigerAbs
	open TigerTree
	open TigerFrame
	
	type level = { depth: int, lstack: label list ref, frame: frame, parent: frame option }
	type access = level * TigerFrame.access
	
	val outermost = {depth=0, lstack = ref [], frame = newFrame (namedlabel "_tigermain", []), parent=NONE }
	
	fun newLevel (parent:level, name, formals) =
		{depth = #depth(parent) + 1, lstack = ref [], frame = newFrame (name, formals), parent = SOME (#frame(parent)) }
	
	fun formals (level:level) = List.map (fn x => (level, x)) (TigerFrame.formals (#frame(level)))
	
	fun allocLocal (level:level) escapes =
		(level, TigerFrame.allocLocal (#frame(level)) escapes)
	
	datatype exp =
		NN
	| Ex of TigerTree.exp (* Expresion *)
	| Nx of TigerTree.stm (* No Result *)
	| Cx of TigerTemp.label * TigerTemp.label -> TigerTree.stm (* Condicion *)

	fun seq [a] = a
		| seq (a::b::[]) = SEQ(a,b)
		| seq (a::xs) = SEQ(a,seq xs)
	
	(* Funciones desempaquetadoras de expresiones *)
	fun unEx (Ex e) = e
		|	unEx (Nx s) = ESEQ (s, CONST 0)
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
		| unEx _ = raise Fail "unEx"
			
	fun unNx (Ex e) = EXP e
		|	unNx (Nx s) = s
		| unNx (Cx c) = 
			let 
				val (lv, lf) = (newlabel(), newlabel())
			in
				seq [c (lv, lf), LABEL lv, LABEL lf]
			end
		| unNx _ = raise Fail "unNx"
	
	fun unCx (Ex (CONST n)) = (fn (lv, lf) => JUMP (NAME (if n = 0 then lf else lv), [lv, lf]))
		|	unCx (Ex e) = (fn (lv, lf) => CJUMP ( NE, e, CONST 0, lv, lf ))
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
			(*procesaString(s,l);*)
			Ex (NAME l)
		end
		
	fun callExp (name, params, proc:bool) =
		if not proc then
			let
				val temp = newtemp()
			in
				Ex (ESEQ (SEQ (EXP (CALL (NAME name, List.map unEx params)), MOVE (TEMP temp, TEMP RV)), TEMP temp))
			end
		else
			Nx (EXP (CALL (NAME name, List.map unEx params)))
	
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
								MOVE (TEMP temp, TEMP RV)), 
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
		
		
end
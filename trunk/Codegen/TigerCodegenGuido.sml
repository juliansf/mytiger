(*
	Codegen para I386.

	Los casos ccte op ctte enteros se tratan aca'.
*)
structure tigercodegen :> tigercodegen =
struct

open tigertree
open tigerassem
open tigerframe 

fun st n = if n<0 then "-"^makestring(~n) else makestring(n)

fun relOp relop =
	case relop of
	EQ =>		"JE "	| NE =>		"JNE "
	| LT =>		"JLT "	| GT =>		"JGT "
	| LE =>		"JLE "	| GE =>		"JGE "
	| ULT =>	"JULT "	| ULE =>	"JULE "
	| UGT =>	"JUGT "	| UGE =>	"JUGE "
(* factor de escala para [base+index*escala] (viene como l2 por shift) *)
fun scaleFact n = n=0 orelse n=1 orelse n=2 orelse n=3
fun sF n = (* conversio'n a factor *)
	case n of
	0 => 1
	| 1 => 2
	| 2 => 4
	| 3 => 8
	| _ => raise Fail "conv. factor incorrecta! (error interno)"
fun codegen frame stm =
	let	val ilist = ref (nil: instr list)
		fun emit x = ilist := x::(!ilist)
		fun result gen = let val t = tigertemp.newtemp() in gen t; t end
		fun munchStm s =
			case s of SEQ(a, b) => (munchStm a; munchStm b)
			| tigertree.MOVE(TEMP t1, MEM(BINOP(PLUS, CONST i, TEMP t2))) =>
				emit(OPER{assem="MOV `d0,M["^st(i)^"+`s0]\n",
					src=[t2], dst=[t1], jump=NONE})
			| tigertree.MOVE(MEM(BINOP(PLUS, CONST i, e)), CONST j) =>
				emit(OPER{assem="MOV M[`s0+"^st(i)^"],"^st(j)^"\n",
					src=[munchExp e], dst=[], jump=NONE})
			| tigertree.MOVE(MEM(BINOP(PLUS, CONST i, e1)), e2) =>
				emit(OPER{assem="MOV M[`s0+"^st(i)^"],`s1\n",
					src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
			| tigertree.MOVE(MEM e1, MEM e2) =>
				let	val e1' = munchExp e1 (* 386 NO tiene M <- M *)
					val e2' = munchExp e2
					val t = tigertemp.newtemp()
				in
					emit(MOVE{assem="MOV `d0,M[`s0]\n", src=e2', dst=t});
					emit(OPER{assem="MOV M[`s1],`s0\n",
						src=[t,e1'], dst=[], jump=NONE})
				end
			| tigertree.MOVE(MEM(CONST i), e2) =>
				emit(MOVE{assem="MOV M[`s0+"^st(i)^"],`s0\n",
					src=munchExp e2, dst="r"})
			| tigertree.MOVE(MEM(e1), CONST i) =>
				emit(OPER{assem="MOV M[`s0],"^st(i)^"\n",
					src=[munchExp e1], dst=[], jump=NONE})
			| tigertree.MOVE(MEM(e1), e2) =>
				emit(OPER{assem="MOV M[`s0],`s1\n",
					src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
			| tigertree.MOVE(TEMP i, CONST j) =>
				emit(MOVE{assem="MOV `d0,"^st(j)^"\n",
					src="", dst=i})
			| tigertree.MOVE(TEMP i, NAME l2) =>
				emit(MOVE{assem="MOV `d0,"^l2^"\n",
					src="", dst=i})
			| tigertree.MOVE(TEMP i, e2) =>
				emit(MOVE{assem="MOV `d0,`s0\n",
					src=munchExp e2, dst=i})
			| tigertree.MOVE(e1, e2) => 
				(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=rv});
				emit(MOVE{assem="MOV `d0,`s0\n",
						src=rv, dst=munchExp e1}))
			| EXP(CALL(NAME n, args)) =>
				(saveCallerSaves();
				emit(OPER{assem="CALL "^n^"\n",
					src=munchArgs(0, rev args),
					dst=calldefs, jump=NONE});
				if length(args)>0 then
					emit(OPER{assem="ADD "^sp^","^st(wSz*length(args))^"\n",
						src=[], dst=[rv]@callersaves, jump=NONE})
				else ();
				restoreCallerSaves())
			| EXP(CALL(e, args)) =>
				(emit(OPER{assem="CALL `s0\n",
					src=munchExp(e)::munchArgs(0, rev args),
					dst=calldefs, jump=NONE});
				if length(args)>0 then
					emit(OPER{assem="ADD `d0,"^st(wSz*length(args))^"\n",
						src=[], dst=[rv]@callersaves, jump=NONE})
				else ();
				restoreCallerSaves())
			| EXP e =>
				emit(MOVE{assem="MOV `d0,`s0\n",
					src=munchExp e, dst=rv})
			| JUMP(NAME n, l) =>
				emit(OPER{assem="JMP "^n^"\n",
						src=[], dst=[], jump=SOME [n]})
			| JUMP(e, l) =>
				let	val e' = munchExp e
				in	emit(OPER{assem="JMP `s0\n",
						src=[e'], dst=l, jump=SOME [e']}) end
			| CJUMP(relop, CONST c1, CONST c2, l1, l2) =>
				let	val que = case relop of
								EQ => c1 = c2 		| NE => c1 <> c2
								| LT => c1 < c2 	| GT => c1 > c2
								| LE => c1 <= c2 	| GE => c1 >= c2
								| ULT => c1 < c2 	| ULE => c1 <= c2
								| UGT => c1 > c2 	| UGE => c1 >= c2
					val l' = if que then l1 else l2
				in	emit(OPER{assem="JMP `s0\n",
						src=[l'], dst=[], jump=SOME [l']}) end
			| CJUMP(relop, CONST c1, e2, l1, l2) =>
				let	val () = emit(OPER{assem="CMP `s0,"^st(c1)^"\n",
						src=[munchExp e2], dst=[], jump=NONE})
				in	emit(OPER{assem=relOp(relop)^l1^"\n", src=[],
						dst=[], jump=SOME[l1, l2]}) end
			| CJUMP(relop, e1, CONST c2, l1, l2) =>
				let	val () = emit(OPER{assem="CMP `s0,"^st(c2)^"\n",
						src=[munchExp e1], dst=[], jump=NONE})
				in	emit(OPER{assem=relOp(relop)^l1^"\n", src=[],
						dst=[], jump=SOME[l1, l2]}) end
			| CJUMP(relop, e1, e2, l1, l2) =>
				let	val () = emit(OPER{assem="CMP `s0,`s1\n",
						src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
				in	emit(OPER{assem=relOp(relop)^l1^"\n", src=[],
						dst=[], jump=SOME[l1, l2]}) end
			| tigertree.LABEL l => emit(LABEL{assem=l^":\n", lab=l})
		and saveCallerSaves() =
			let	fun emitcdefs s =
					emit(OPER{assem="PUSH `s0\n", src=[s],
								dst=[], jump=NONE})
			in	List.map emitcdefs tigerframe.callersaves end
		and restoreCallerSaves() =
			let	fun emitcdefs s =
					emit(OPER{assem="POP `d0\n", src=[],
								dst=[s], jump=NONE})
			in	List.app emitcdefs (rev tigerframe.callersaves) end
		and munchArgs(_, []) = []
		| munchArgs(i, h::t) = 
			let	val (instr, e) =
					case h of
					CONST i => (OPER{assem="PUSH "^st(i)^"\n",
								src=[], dst=[], jump=NONE}, "")
					| NAME n => (OPER{assem="PUSH "^n^"\n",
								src=[], dst=[], jump=NONE}, "")
					| TEMP n => (OPER{assem="PUSH `s0\n",
								src=[n], dst=[], jump=NONE}, "")
					| MEM(TEMP n) =>
								(OPER{assem="PUSH M[`s0]\n",
								src=[n], dst=[], jump=NONE}, "")
					| MEM(BINOP(PLUS, CONST c, TEMP n)) =>
								(OPER{assem="PUSH M[`s0+"^st(c)^"]\n",
								src=[n], dst=[], jump=NONE}, "")
					| MEM(BINOP(PLUS, TEMP n, CONST c)) =>
								(OPER{assem="PUSH M[`s0+"^st(c)^"]\n",
								src=[n], dst=[], jump=NONE}, "")
					| _ =>	let	val e = munchExp h
							in	(OPER{assem="PUSH `s0\n", src=[e],
									dst=[], jump=NONE}, e) end
			in	emit(instr);
				if e<>"" then e::munchArgs(i+1, t) else munchArgs(i+1, t)
			end
		and munchExp e =
			case e of
			CONST i => st i
			| TEMP t => t
			| NAME l => l
			| MEM(BINOP(PLUS, CONST j, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^st(i+j)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(PLUS, NAME n, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^n^"+"^st(i)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(PLUS, CONST i, NAME n)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^n^"+"^st(i)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(PLUS, e1, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0+"^st(i)^"]\n",
						src=munchExp e1, dst=r}))
			| MEM(BINOP(PLUS, CONST i, e1)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0+"^st(i)^"]\n",
						src=munchExp e1, dst=r}))
			| MEM(BINOP(PLUS, TEMP t0, BINOP(LSHIFT, TEMP t1, CONST i))) =>
				if scaleFact i then
					result(fn r =>
						emit(OPER{assem="MOV `d0,M[`s0+"^st(sF i)^"*`s1]\n",
							src=[t0,t1], dst=[r], jump=NONE}))
				else
					result(fn r =>
						(emit(MOVE{assem="MOV `d0,`s0\n",
							src=t1, dst=r});
						emit(OPER{assem="SHL `d0,"^st(i)^"\n",
							src=[], dst=[r], jump=NONE});
						emit(MOVE{assem="MOV `d0,M[`s0]\n",
							src=r, dst=r})))
			| MEM(BINOP(MINUS, CONST j, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^st(j-i)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(MINUS, NAME n, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^n^"-"^st(i)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(MINUS, CONST i, NAME n)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M["^n^"-"^st(i)^"]\n",
						src="", dst=r}))
			| MEM(BINOP(MINUS, e1, CONST i)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0-"^st(i)^"]\n",
						src=munchExp e1, dst=r}))
			| MEM(BINOP(MINUS, CONST i, e1)) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0-"^st(i)^"]\n",
						src=munchExp e1, dst=r}))
			| MEM(CONST i) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0+"^st(i)^"]\n",
						src=st i, dst=r}))
			| MEM(e1) =>
				result(fn r =>
					emit(MOVE{assem="MOV `d0,M[`s0+0]\n",
						src=munchExp e1, dst=r}))
			| BINOP(PLUS, e1, CONST i) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="ADD `d0,"^st(i)^"\n",
						src=[], dst=[r], jump=NONE})))
			| BINOP(PLUS, CONST i, e1) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="ADD `d0,"^st(i)^"\n",
						src=[], dst=[r], jump=NONE})))
			| BINOP(PLUS, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="ADD `d0,`s0\n",
						src=[munchExp e2], dst=[r], jump=NONE})))
			| BINOP(MINUS, e1, CONST i) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="ADD `d0,"^st(i)^"\n",
						src=[], dst=[r], jump=NONE})))
		(* el caso especial de 0-exp, generado por el parser *)
			| BINOP(MINUS, CONST 0, e1) =>
				result(fn r =>
					let	val src=munchExp e1
					in
						emit(OPER{assem="NEG `d0\n",
							src=[], dst=[src], jump=NONE});
						emit(MOVE{assem="MOV `d0,`s0\n",
							src=src, dst=r})
					end)
			| BINOP(MINUS, CONST i, e1) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,"^st(i)^"\n",
						src="", dst=r});
					emit(OPER{assem="SUB `d0,`s0\n",
						src=[munchExp e1], dst=[r], jump=NONE})))
			| BINOP(MINUS, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="SUB `d0,`s0\n",
						src=[munchExp e2], dst=[r], jump=NONE})))
			| BINOP(MUL, e1, CONST i) =>
				result(fn r =>
					emit(OPER{assem="MUL `d0,`s0*"^st(i)^"\n",
						src=[munchExp e1], dst=[r], jump=NONE}))
			| BINOP(MUL, CONST i, e1) =>
				result(fn r =>
					emit(OPER{assem="MUL `d0,`s0*"^st(i)^"\n",
						src=[munchExp e1], dst=[r], jump=NONE}))
			| BINOP(MUL, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOVE `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="MUL `d0,`s0\n",
						src=[munchExp e2, r], dst=[r], jump=NONE})))
			(* arghhh!! Intel y la @#! *)
			| BINOP(DIV, e, CONST i) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n", src=munchExp e, dst=rv});
					emit(OPER{assem="CWQ\n", src=[rv], dst=[ov], jump=NONE});
					emit(MOVE{assem="MOV `d0,"^st(i)^"\n", src="", dst=r});
					emit(OPER{assem="DIV `s0\n",
						src=[r], dst=[rv,ov], jump=NONE});
					emit(MOVE{assem="MOV `d0,`s0\n", src=rv, dst=r})))
			| BINOP(DIV, CONST i, e) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,"^st(i)^"\n", src="", dst=rv});
					emit(OPER{assem="CWQ `s0\n", src=[rv],
						dst=[ov], jump=NONE});
					emit(MOVE{assem="MOV `d0,`s0\n", src=munchExp e, dst=r});
					emit(OPER{assem="DIV `s0\n",
						src=[r], dst=[rv,ov], jump=NONE});
					emit(MOVE{assem="MOV `d0,`s0\n", src=rv, dst=r})))
			| BINOP(DIV, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n", src=munchExp e1, dst=rv});
					emit(OPER{assem="CWQ `s0\n", src=[rv],
						dst=[ov], jump=NONE});
					emit(OPER{assem="DIV `s0/`s1\n",
						src=[rv, munchExp e2], dst=[rv,ov], jump=NONE});
					emit(MOVE{assem="MOV `d0,`s0\n", src=rv, dst=r})))
			| BINOP(AND, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="AND `d0,`s1\n",
						src=[munchExp e2,r], dst=[r], jump=NONE})))
			| BINOP(OR, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="OR `d0,`s1\n",
						src=[munchExp e2,r], dst=[r], jump=NONE})))
			| BINOP(LSHIFT, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="LSH `d0,`s0\n",
						src=[munchExp e2,r], dst=[r], jump=NONE})))
			| BINOP(RSHIFT, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="RSH `d0,`s1\n",
						src=[munchExp e2,r], dst=[r], jump=NONE})))
			| BINOP(ARSHIFT, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="RSA `d0,`s0\n",
						src=[munchExp e2], dst=[r], jump=NONE})))
			| BINOP(XOR, e1, e2) =>
				result(fn r =>
					(emit(MOVE{assem="MOV `d0,`s0\n",
						src=munchExp e1, dst=r});
					emit(OPER{assem="XOR `d0,`s1\n",
						src=[munchExp e2,r], dst=[r], jump=NONE})))
			| CALL(exp, explist) =>
				result(fn r =>
					munchStm(tigertree.EXP(CALL(exp, explist))))
			| ESEQ(stm, exp) => raise Fail "ESEQ incompleto!"
	in	munchStm stm; rev(!ilist) end

end

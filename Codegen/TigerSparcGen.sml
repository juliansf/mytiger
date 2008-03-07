structure TigerSparcGen :> TigerSparcGen =
struct

open TigerTree
open TigerFrame 
open TigerAssem
open TigerError

structure T = TigerTree
structure A = TigerAssem

fun st n = if n<0 then "-"^makestring(~n) else makestring(n)

fun relOp relop =
	case relop of EQ =>		"be "	| NE =>		"bne "
            	| LT =>		"bl "	| GT =>		"bg "
            	| LE =>		"ble "	| GE =>		"bge "
            	| ULT =>	"blu "	| ULE =>	"bleu "
            	| UGT =>	"bgu "	| UGE =>	"bgeu "

fun codegen frame stm =
    let
        (* Lista de las instrucciones en assembler para SPARC V9*)
        val ilist = ref (nil:instr list)

        (* Funcion que guarda las instrucciones emitidas *)
        fun emit i = ilist := i::(!ilist)

        (* Selección de instrucciones usando Maximal Munch *)        
        fun munchStm (SEQ(a,b)) = (munchStm a; munchStm b)
          | munchStm (T.MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2)) =
                emit (OPER {assem="st `s1, [`s0 + " ^st(i)^ "]\n",
                            dst=[], src=[munchExp e1, munchExp e2], jump=NONE })

          | munchStm (T.MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2)) =
                emit (OPER {assem="st `s1, [`s0 + " ^st(i)^ "]\n",
                            dst=[], src=[munchExp e1, munchExp e2], jump=NONE })

          | munchStm (T.MOVE (MEM (e1), MEM (e2))) =
                let
                    val e1' = munchExp e1
                    val e2' = munchExp e2
                    val t:TigerTemp.temp = TigerTemp.newtemp()
                in
                   emit(A.MOVE {assem="ld [`s0], `d0\n", src=e2', dst=t});
                   emit(OPER {assem="st `s0, [`s1]\n", src=[t,e1'], dst=[], jump=NONE})  
                end

          | munchStm (T.MOVE (MEM (CONST i), e2)) =
                emit (OPER {assem="st `s0, ["^st(i)^"]\n",
                            src=[munchExp e2], dst=[], jump=NONE })

          | munchStm (T.MOVE (MEM (e1), e2)) =
				emit (OPER {assem="st `s1, [`s0]\n",
                            src=[munchExp e1, munchExp e2], dst=[], jump=NONE })

          | munchStm (T.MOVE (TEMP i, MEM (BINOP (PLUS, e1, CONST j)))) =
                emit (OPER {assem="ld [`s0 + " ^st(j)^ "], `d0\n",
                            src=[munchExp e1], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, MEM (BINOP (PLUS, CONST j, e1)))) =
                emit (OPER {assem="ld [`s0 + " ^st(j)^ "], `d0\n",
                            src=[munchExp e1], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, MEM (e2))) = 
                emit (OPER {assem="ld `s0, `d0\n",
                            src=[munchExp e2], dst=[i], jump=NONE})
        
		  (* !!! ESTA COMENTADO PARA QUE NO UTILIZE EL REGISTRO G0 !!! *)
		  (* UNA VEZ RESUELTO COMO EVITAR COALESCER EL G0 DESCOMENTAR *)        
(*		  | munchStm (T.MOVE (TEMP i, CONST 0)) =
                emit (A.MOVE {assem="mov `s0, `d0\n", src=G0, dst=i}) *)

          | munchStm (T.MOVE (TEMP i, CONST j)) =
                emit (OPER {assem="set "^ st(j) ^", `d0\n",
                            src=[], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, e2)) =
                emit (A.MOVE {assem="mov `s0, `d0\n", src=munchExp e2, dst=i})

          | munchStm (T.LABEL lab) =
                emit(A.LABEL {assem=(TigerTemp.labelname lab) ^":\n", lab=lab})

          (* Código para llamada a procedimiento, no hay valor de retorno *)        
          | munchStm (EXP (CALL (e, args))) =
                emit (OPER {assem="call `s0\n",
                            src=munchExp(e)::munchArgs(0, args), dst=calldefs, jump=NONE})

          | munchStm (EXP (e)) =
				emit(A.MOVE {assem="mov `s0,`d0\n",	src=munchExp e, dst=RV})

          | munchStm (JUMP (T.NAME lab, labels)) =
                emit(OPER {assem="ba " ^ TigerTemp.labelname(lab) ^ "\n",
                           src=[], dst=[], jump=SOME labels} )

          | munchStm (JUMP (e, labels)) =
                emit(OPER {assem="ba `s0\n",
                           src=[munchExp e], dst=[], jump=SOME labels} )

          | munchStm (CJUMP (relop, e1, e2, lv, lf)) = (
                emit(OPER {assem="cmp `s0, `s1\n",
                           src=[munchExp e1, munchExp e2], dst=[], jump=NONE});
                emit(OPER {assem=(relOp relop) ^ TigerTemp.labelname(lv) ^ "\n", 
                           src=[], dst=[], jump=SOME [lv,lf]}))
          
          | munchStm _ = Error (ErrorInternalError "TigerSparcGen.munchStm pattern matching incompleto", 0)

        and munchArgs(_,[]) = []
          | munchArgs(i, h::t) = 
            (* Si i > 5, tenemos argumentos que van en el stack *)
            if i < 6 then
                let
                    val d = List.nth(argregs,i)
                in
                    emit(A.MOVE {assem="mov `s0, `d0\n", src=munchExp h, dst=d});
                    d::munchArgs(i+1, t)
                end
            else
  	            (emit(OPER {assem="st `s0, [sp + "^ st(i * wordSize + 128 + stackBias) ^"]\n", 
  	            		    src=[munchExp h], dst=[], jump=NONE});
  	            munchArgs(i+1,t))
  
        and result(gen) = let val t = TigerTemp.newtemp() in gen t; t end

        and munchExp(CONST j) =
            result(fn r =>
                emit (OPER {assem="set "^ st(j) ^", `d0\n",
                            src=[], dst=[r], jump=NONE}))            
        
          | munchExp(NAME l) =
            result(fn r =>
                emit (OPER {assem="ld " ^ TigerTemp.labelname(l) ^ ", `d0\n",
                            src=[], dst=[r], jump=NONE}))
                            
          | munchExp(TEMP t) = t
        
          | munchExp(MEM (BINOP (PLUS, e1, CONST i))) =
            result(fn r => 
                emit (OPER {assem="ld [`s0 + " ^st(i)^ "], `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(MEM (BINOP (PLUS, CONST i, e1))) =
            result(fn r => 
                emit (OPER {assem="ld [`s0 + " ^st(i)^ "], `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
 
          | munchExp(MEM (CONST i)) =
            result(fn r =>
                emit (OPER {assem="ld ["^st(i)^"], `d0\n",
                            src=[], dst=[r], jump=NONE}))

          | munchExp(MEM (e1)) =
            result(fn r =>
                emit (OPER {assem="ld [`s0], `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (PLUS, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="add `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (PLUS, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="add `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (PLUS, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="add `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (MINUS, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="sub `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (MINUS, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="sub `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (MINUS, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="sub `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (MUL, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="smul `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (MUL, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="smul `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (MUL, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="smul `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (DIV, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="sdiv `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (DIV, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="sdiv `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (DIV, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="sdiv `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (AND, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="and `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (AND, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="and `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (AND, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="and `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (OR, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="or `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (OR, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="or `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (OR, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="or `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (XOR, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="xor `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (XOR, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="xor `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (XOR, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="xor `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (LSHIFT, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="sll `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (LSHIFT, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="sll `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (LSHIFT, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="sll `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (RSHIFT, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="srl `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (RSHIFT, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="srl `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (RSHIFT, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="srl `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp(BINOP (ASHIFT, e1, CONST i)) =
            result(fn r =>
                emit (OPER {assem="sra `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
          
          | munchExp(BINOP (ASHIFT, CONST i, e1)) =
            result(fn r =>
                emit (OPER {assem="sra `s0, "^st(i)^", `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(BINOP (ASHIFT, e1, e2)) =
            result(fn r =>
                emit (OPER {assem="sra `s0, `s1, `d0\n",
                            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                     
          | munchExp(ESEQ _) = Error (ErrorInternalError "No debería llegar un ESEQ en TigerSparcGen.munchExp", 0)
          
          | munchExp _ = Error (ErrorInternalError "TigerSparcGen.munchExp pattern matching incompleto", 0)
    in
        munchStm stm;
        rev(!ilist)
    end

end

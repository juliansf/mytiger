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
                emit (OPER {assem="stx `s1, [`s0 + " ^st(i)^ "]\n",
                            dst=[], src=[munchExp e1, munchExp e2], jump=NONE })

          | munchStm (T.MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2)) =
                emit (OPER {assem="stx `s1, [`s0 + " ^st(i)^ "]\n",
                            dst=[], src=[munchExp e1, munchExp e2], jump=NONE })

          | munchStm (T.MOVE (MEM (e1), MEM (e2))) =
                let
                    val e1' = munchExp e1
                    val e2' = munchExp e2
                    val t:TigerTemp.temp = TigerTemp.newtemp()
                in
                   emit(A.MOVE {assem="ldx [`s0], `d0\n", src=e2', dst=t});
                   emit(OPER {assem="stx `s0, [`s1]\n", src=[t,e1'], dst=[], jump=NONE})  
                end

          | munchStm (T.MOVE (MEM (CONST i), e2)) =
                emit (OPER {assem="stx `s0, ["^st(i)^"]\n",
                            src=[munchExp e2], dst=[], jump=NONE })

          | munchStm (T.MOVE (MEM (e1), e2)) =
				emit (OPER {assem="stx `s1, [`s0]\n",
                            src=[munchExp e1, munchExp e2], dst=[], jump=NONE })

          | munchStm (T.MOVE (TEMP i, MEM (BINOP (PLUS, e1, CONST j)))) =
                emit (OPER {assem="ldx [`s0 + " ^st(j)^ "], `d0\n",
                            src=[munchExp e1], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, MEM (BINOP (PLUS, CONST j, e1)))) =
                emit (OPER {assem="ldx [`s0 + " ^st(j)^ "], `d0\n",
                            src=[munchExp e1], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, MEM (e2))) = 
                emit (OPER {assem="ldx [`s0], `d0\n",
                            src=[munchExp e2], dst=[i], jump=NONE})
        
		  (* Utilizamos un OPER en lugar de un MOVE para evitar que el registro g0 
		  	 haga coalescing con i en el coloreo. *)        
		  | munchStm (T.MOVE (TEMP i, CONST 0)) =
                emit (OPER {assem="mov `s0, `d0\n", src=[G0], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, CONST j)) =
                emit (OPER {assem="set "^ st(j) ^", `d0\n",
                            src=[], dst=[i], jump=NONE})

          | munchStm (T.MOVE (TEMP i, e2)) =
                emit (A.MOVE {assem="mov `s0, `d0\n", src=munchExp e2, dst=i})

          | munchStm (T.LABEL lab) =
                emit(A.LABEL {assem=(TigerTemp.labelname lab) ^":\n", lab=lab})

          (* Código para llamada a procedimiento, no hay valor de retorno *)
          | munchStm (EXP (CALL (NAME l, args))) =
          		let
	          		val max = getMaxCallArgs (frame)
	          	in
                	emit (OPER {assem="call "^(TigerTemp.labelname l)^"\n",
                            src=munchArgs(0, args), dst=calldefs, jump=NONE});
				(* Calculamos el max nº de args de un call y lo guardamos en el frame para que 
				   procEntryExit3 haga el save utilizando dicho valor *)                    
                 	if List.length(args) > (!max) then max := List.length(args) else ();
					emit (OPER {assem=" nop\n",src=[],dst=[],jump=NONE})
				end 

          | munchStm (EXP (e)) =
				emit(A.MOVE {assem="mov `s0,`d0\n",	src=munchExp e, dst=O0})

          | munchStm (JUMP (T.NAME lab, labels)) =
                emit(OPER {assem="ba " ^ TigerTemp.labelname(lab) ^ "\n nop\n",
                           src=[], dst=[], jump=SOME labels} )

          | munchStm (JUMP (e, labels)) =
                emit(OPER {assem="ba `s0\n nop\n",
                           src=[munchExp e], dst=[], jump=SOME labels} )

          | munchStm (CJUMP (relop, e1, e2, lv, lf)) = (
                emit(OPER {assem="cmp `s0, `s1\n",
                           src=[munchExp e1, munchExp e2], dst=[], jump=NONE});
                emit(OPER {assem=(relOp relop) ^ TigerTemp.labelname(lv) ^ "\n nop\n", 
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
  	            (emit(OPER {assem="stx `s0, [%sp + "^ st(i * wordSize + 128 + stackBias) ^"]\n", 
  	            		    src=[munchExp h], dst=[], jump=NONE});
  	            munchArgs(i+1,t))
  
        and result(gen) = let val t = TigerTemp.newtemp() in gen t; t end

        and munchExp(CONST j) =
            result(fn r =>
                emit (OPER {assem="set "^ st(j) ^", `d0\n",
                            src=[], dst=[r], jump=NONE}))            
        
          | munchExp(NAME l) =
          	let val t = TigerTemp.newtemp()
          	in
	            result(fn r => (emit (OPER {assem="sethi %hi(" ^ TigerTemp.labelname(l) ^ "), `d0\n",
                    		        src=[], dst=[t], jump=NONE});
                    		    emit (OPER {assem="or `s0, %lo(" ^ TigerTemp.labelname(l) ^ "), `d0\n",
                    		        src=[t], dst=[r], jump=NONE})))
          	end

                            
          | munchExp(TEMP t) = t
        
          | munchExp(MEM (BINOP (PLUS, e1, CONST i))) =
            result(fn r => 
                emit (OPER {assem="ldx [`s0 + " ^st(i)^ "], `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))

          | munchExp(MEM (BINOP (PLUS, CONST i, e1))) =
            result(fn r => 
                emit (OPER {assem="ldx [`s0 + " ^st(i)^ "], `d0\n",
                            src=[munchExp e1], dst=[r], jump=NONE}))
 
          | munchExp(MEM (CONST i)) =
            result(fn r =>
                emit (OPER {assem="ldx ["^st(i)^"], `d0\n",
                            src=[], dst=[r], jump=NONE}))

          | munchExp(MEM (e1)) =
            result(fn r =>
                emit (OPER {assem="ldx [`s0], `d0\n",
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

	fun literals litList =
	let
		val tmp = ref ""
		
		fun size s = Int.toString (String.size s)
		
		fun format lab size str = 
			".align 8\n"^ lab ^":\n"^ 
			".xword "^ size ^"\n"^
			".asciz \""^ str ^"\"\n"

		(* Para imprimir los literales, transformamos el string escapado "str" al real para poder calcular
		   su largo real y después lo emitimos escapándolo a la C que es lo que espera el gnu assembler.*)
		fun aux (TigerCanon.LITERAL (lab,str)) =
			let
				val str' = valOf (String.fromString str)
				handle Option => Error (ErrorInternalError "Error interno en TigerSparcGen:literals:aux\n",0)
			in
				tmp := (!tmp) ^ (format (TigerTemp.labelname lab) (size str') (String.toCString str'))
			end

		  | aux _ = Error (ErrorInternalError "Error interno en TigerSparcGen.sml:literals", 0)
	in
		List.app aux litList;
		".section \".rodata\"\n" ^ (!tmp) ^ "\n"
	end
	
	fun startCodeEmition () =
		(print ".section \".text\"\n";
		 print ".register %g2, #scratch\n";
		 print ".register %g3, #scratch\n")
end

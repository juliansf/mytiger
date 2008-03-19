(*
                    low addresses

       %sp --> +-------------------------+  --         
               | 16 dwords for storing   |   |
               | LOCAL and IN registers  |   |
   %sp+128 --> +-------------------------+	 |  prologo obligatorio
    		   |   6 dwords for callee   |	 |  
               |   to store register     |	 |
               |       arguments         |	 |
   %sp+176 --> +-------------------------+  --
    		   |  outgoing parameters    |
               |  past the 6th, if any   |
               +-------------------------+
               |  space, if needed, for  |
               |  compiler temporaries   |
               |   and saved floating-   |
               |    point registers      |
               +-------------------------+
						   ...
               +-------------------------+
               |    space dynamically    |
               |    allocated via the    |
               |  alloca() library call  |
               +-------------------------+
               |  space, if needed, for  |
               |    automatic arrays,    |
               |    aggregates, and      |
               |   addressable scalar    |
               |       automatics        |
       %fp --> +-------------------------+

                     high addresses

*)

structure TigerSparcFrame :> TigerSparcFrame =
struct
	open TigerTemp
	open tigertab
	open TigerTree
	open TigerError

	type register = string

	datatype access = InFrame of int | InReg of TigerTemp.temp

	type frame = {localOffset: int ref, formals: access list, label: TigerTemp.label, maxCallArgs: int ref}
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
							| STRING of TigerTemp.label * string
	
	(* Registros del SparcV9 *)

    val G0 = namedtemp("g0")	(* Zero Register *)
    val G1 = namedtemp("g1")
    val G2 = namedtemp("g2")
    val G3 = namedtemp("g3")
    val G4 = namedtemp("g4")
    val G5 = namedtemp("g5")

    val O0 = namedtemp("o0")	(* Return Value *)
    val O1 = namedtemp("o1")
    val O2 = namedtemp("o2")
    val O3 = namedtemp("o3")
    val O4 = namedtemp("o4")
    val O5 = namedtemp("o5")
    val SP = namedtemp("sp")    (* Stack Pointer *)
    val O7 = namedtemp("o7")   
    
    val L0 = namedtemp("l0")	(* L0 a L7 de uso local *)
    val L1 = namedtemp("l1")
    val L2 = namedtemp("l2")
    val L3 = namedtemp("l3")
    val L4 = namedtemp("l4")
    val L5 = namedtemp("l5")
    val L6 = namedtemp("l6")
    val L7 = namedtemp("l7")    
    
    val I0 = namedtemp("i0")
    val I1 = namedtemp("i1")
    val I2 = namedtemp("i2")
    val I3 = namedtemp("i3")
    val I4 = namedtemp("i4")
    val I5 = namedtemp("i5")
    val FP = namedtemp("fp")    (* Frame Pointer *)
    val I7 = namedtemp("i7")    (* Return Address - 8*)
      
	val specialregs = [O0, FP, SP, I7, G0]
	val argregs = [O0, O1, O2, O3, O4, O5]
	val inputRegs = [I0,I1,I2,I3,I4,I5]
	val calleesaves = [G2, G3]
	val callersaves = [G1, G4, G5]
    val calldefs = [O0, O1, O2, O3, O4, O5, O7] @ callersaves

	val wordSize = 8
	val prologSize = 176					(* 16 dwords (regs in y local) + 6 dwords (regs in) *)
	val incrLocal = wordSize
	val stackBias = 2047
	val slOffset = 128						(* El offset del SL es el 1er dword para los register args *)

	(* Esta sección es utilizada por el algoritmo de coloreo *)	
	val precolored = specialregs @ [I0,I1,I2,I3,I4,I5,O1,O2,O3,O4,O5,O7]
    val registerlist = precolored @ [G1,G2,G3,G4,G5,L0,L1,L2,L3,L4,L5,L6,L7]
          
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

    (* Crea un nuevo frame y designa registros/stack a sus parámetros formales. *)
	fun newFrame (name, formals) =
		let
		    val inputRegs = ref inputRegs  (* Los primeros 5 estan en registros *)
			val argsoffset = ref 128			    			

			fun processFormals arg =
			    let
			        val access = case arg of
				     			     true  => InFrame(!argsoffset) 
			   				       | false => case !inputRegs of
				    			                  [] => InFrame (!argsoffset)
				    			                | r::rs => InReg r
			    in
				   (argsoffset := !argsoffset + incrLocal;
				    inputRegs := tl (!inputRegs) handle Empty => ();
				    access)
				end
        in
		    (* true::formals es para dejar como 1er arg al static link *)											
			{localOffset = ref 0,
			 formals = List.map processFormals (true::formals),
			 label = name,
			 maxCallArgs = ref 0}		
		end
			
	(* Devuelve una lista con los access de cada parámetro formal del frame *)
	fun formals {formals, localOffset, label, maxCallArgs} = formals	
	fun name {formals, localOffset, label, maxCallArgs} = label
	
	(* Aloca una variable local en el frame pasado como argumento *)
	fun allocLocal {localOffset, formals, label, maxCallArgs} escapes =
		if escapes then
		    (localOffset := !localOffset - incrLocal; InFrame(!localOffset) )
		else InReg (newtemp())
									 
	(* Devuelve el código intermedio para acceder a una variable del frame 
	   El segundo argumento "sb" es el stackBias que le tiene que sumar al offset 
	   de la variable. En caso de que stackAddr sea el código para recorrer los 
	   SL el stackBias ya está sumado en la primer desreferencia del FP, 
	   por lo que "sb" es 0. *)
    fun exp (InReg r) _ _ = TEMP r
      | exp (InFrame offset) sb stackAddr = MEM (BINOP (PLUS, CONST (offset + sb), stackAddr))

	(* Genera un fragmento para los string *)
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun getFrameLabel (frame:frame) = #label(frame)
	
	fun getLocalOffset (frame:frame) = !(#localOffset(frame))
	
	fun getMaxCallArgs (frame:frame) = #maxCallArgs(frame)
	
	fun getAccessOffset (acc:access) =
		case acc of
	    	InFrame x => x
		  | _ => Error (ErrorInternalError "Error interno en TigerSparcFrame.sml:getAccessOffset u\n",0)
	
	fun procEntryExit1 (body, {formals, localOffset, label, maxCallArgs}) =
	    let
			val calleesavesTemps = List.map (fn r => (newtemp(),r)) calleesaves
			val usedInputRegs = ListPair.zip (inputRegs,formals)
			
			fun aux (ir, access) = case access of
								       InReg r => MOVE (TEMP (newtemp()), TEMP r)
									 | InFrame off => MOVE (MEM (BINOP (PLUS, TEMP FP, CONST (off+stackBias))), TEMP ir)

			val argsMoves = List.map aux usedInputRegs
	    	val (entry,exit) = ListPair.unzip (List.map (fn (t,r)=>(MOVE (TEMP t, TEMP r),MOVE (TEMP r, TEMP t))) calleesavesTemps)
 
		in
			seq (entry @ argsMoves @ [body] @ exit) 
	    end
	    
	fun procEntryExit2 (frame, body) =
	    	body @ [TigerAssem.OPER {assem="", src=[G0, I7, SP]@calleesaves, dst=[], jump=SOME[]}]

	fun procEntryExit3 (frame, linstr) = 
		let
			fun st n = if n<0 then "-"^makestring(~n) else makestring(n)
			
			val maxArgs = !(getMaxCallArgs(frame))
			
			val totalOffset = prologSize + ~(getLocalOffset(frame)) + (if maxArgs > 6 then (maxArgs - 6) * wordSize else 0)
			(* El stacksize tiene que ser multiplo de 16, le sumamos el resto de div por 16 para que de *)
			val stacksize = ~(totalOffset + (totalOffset mod 16))
			val label = labelname(getFrameLabel frame)
			val prolog = ".align 4\n.global "^label^"\n"^label^":\n"
			val entry = TigerAssem.OPER {assem="save `s0, "^ st stacksize ^", `d0\n",
										 src=[SP],
										 dst=[SP],
										 jump=NONE}
			val exit = "return %i7+8\nnop\n.size "^label^", .-"^label^"\n\n"

		in
			{prolog=prolog, body=entry::linstr, epilogue=exit}	
		end
	
	

end

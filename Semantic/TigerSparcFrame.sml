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

	type register = string

	datatype access = InFrame of int | InReg of TigerTemp.temp

	type frame = {localOffset: int ref, formals: access list, label: TigerTemp.label}
	
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
    val O6 = namedtemp("sp")    (* Stack Pointer *)
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
    val I6 = namedtemp("fp")    (* Frame Pointer *)
    val I7 = namedtemp("i7")    (* Return Address *)
    
	(* Algunos alias *)
    val R0 = G0
	val FP = I6
	val SP = O6
	val RV = O0
	val RA = I7
   
	val specialregs = [RV, FP, SP, RA, R0]
	val argregs = [O0, O1, O2, O3, O4, O5]
	val calleesaves = [G2, G3]
	val callersaves = [G1, G4, G5]
    val calldefs = RV :: callersaves

	val wordSize = 8
	val prologSize = 176					(* 16 dwords (regs in y local) + 6 dwords (regs in) *)
	val incrLocal = wordSize
	val stackBias = 2047
	val slOffset = 128						(* El offset del SL es el 1er dword para los register args *)

	(* Esta sección es utilizada por el algoritmo de coloreo *)	
	val precolored = [FP,SP,R0,RA,RV,I0,I1,I2,I3,I4,I5,I6,I7]    (*Ojo acá están duplicados I6 e I7*)
    val registerlist = [G0,G1,G2,G3,G4,G5,O0,O1,O2,O3,O4,O5,O6,O7,
    			        L0,L1,L2,L3,L4,L5,L6,L7,I0,I1,I2,I3,I4,I5,I6,I7]
          
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

    (* Crea un nuevo frame y designa registros/stack a sus parámetros formales. *)
	fun newFrame (name, formals) =
		let
		    val inputRegs = ref [I0,I1,I2,I3,I4,I5]  (* Los primeros 5 estan en registros *)
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
			{localOffset = ref 0, formals = List.map processFormals (true::formals), label = name}		
		end
			
	(* Devuelve una lista con los access de cada parámetro formal del frame *)
	fun formals {localOffset, formals, label} = formals
	
	fun name {localOffset, formals, label} = label
	
	(* Aloca una variable local en el frame pasado como argumento *)
	fun allocLocal {localOffset, formals, label} escapes =
		if escapes then
		    (localOffset := !localOffset - incrLocal; InFrame(!localOffset) )
		else InReg (newtemp())
									 
	(* Devuelve el código intermedio para acceder a una variable del frame *)
    fun exp (InReg r) _ = TEMP r
      | exp (InFrame offset) stackAddr = MEM (BINOP (PLUS, CONST offset, stackAddr))

	(* Genera un fragmento para los string *)
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun getFrameLabel (frame:frame) = #label(frame)
	
	fun procEntryExit1 (body, frame) =
	    let
			val funlabel = getFrameLabel(frame)

	    	val entry = [ LABEL funlabel,
					  	  SEQ( LABEL (namedlabel "Prologo"),
						       MOVE (MEM (BINOP (PLUS, TEMP FP, CONST (slOffset + stackBias))), TEMP I0))]

			val exit = [SEQ( LABEL (namedlabel "Epilogo"),
							 MOVE (MEM (TEMP SP), TEMP FP))]
		in
			seq (entry @ [body] @ exit) 
	    end
	    
    (*!!! OJO VER EL jump=SOME[] !!! pa' que mierda está?*)
	fun procEntryExit2 (frame, body) =
	    body @ [TigerAssem.OPER {assem="", src=[R0, RA, SP]@calleesaves, dst=[], jump=SOME[]}]
	    
(*	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end
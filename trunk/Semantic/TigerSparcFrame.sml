(*
                    low addresses

               +-------------------------+  --         
     %sp  -->  | 16 dwords for storing   |   |
               | LOCAL and IN registers  |   |
               +-------------------------+   |
   %sp+128 --> |  one-dword pointer to   |   |
               | aggregate return value  |   |  190 bytes, reservo 192 para
               +-------------------------+   |  que quede alineado en una dir
   %sp+136 --> |   6 dwords for callee   |   |  múltiplo de 8.
               |   to store register     |   |
               |       arguments         |   |
               +-------------------------+  --
   %sp+176 --> |  outgoing parameters    |
               |  past the 6th, if any   |
               +-------------------------+
               |  space, if needed, for  |
               |  compiler temporaries   |
               |   and saved floating-   |
               |    point registers      |
               +-------------------------+

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
               +-------------------------+
    %fp  -->
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
	
    val R0 = namedtemp("r0")
	val FP = namedtemp("fp")
	val SP = namedtemp("sp")
	val RV = namedtemp("rv")
	val RA = namedtemp("ra")
    val G1 = namedtemp("g1")
    val G2 = namedtemp("g2")
    val G3 = namedtemp("g3")
    val G4 = namedtemp("g4")
    val G5 = namedtemp("g5")

    val O0 = namedtemp("o0")
    val O1 = namedtemp("o1")
    val O2 = namedtemp("o2")
    val O3 = namedtemp("o3")
    val O4 = namedtemp("o4")
    val O5 = namedtemp("o5")
   
	val specialregs = [RV, FP, SP, RA, R0]
	val argregs = [O0, O1, O2, O3, O4, O5]
	val calleesaves = [G2, G3]
	val callersaves = [G1, G4, G5]

    val calldefs = RV :: RA :: callersaves          (*  Realmente se sobreescribe el RA en SPARC ?*)
	val wordSize = 8
	val prologSize = wordSize 
	val incrLocal = ~wordSize
	val slOffset = 0						(* Cuando defina la estructura del frame en SPARC 
												   definir la posición exacta del static link.    *)
    
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

    (* Crea un nuevo frame y designa registros/stack a sus parámetros formales (argumentos). *)
    (* VER argoffset, está al pedo!!!! NO HAY QUE UTILIZAR localOffset ???? *)
    (* !!! MODIFICAR para que tenga en cuenta que los primeros 7 argumentos vienen en registros
           y el resto van en stack !!! *)
	fun newFrame (name, formals) =
		let
			val argsoffset = ref 0
			fun processFormals (arg, argList) = 
				case arg of
				     true  => (argsoffset := !argsoffset - incrLocal; InFrame(!argsoffset) :: argList) 
				    |false => InReg (newtemp()) :: argList
        in
		    (*true::formals es para dejar como 1er arg al static link*)											
			{localOffset = ref 0, formals = List.foldl processFormals [] (true::formals), label = name}		
		end
			
	(* Devuelve una lista con los access de cada parámetro formal del frame *)
	fun formals {localOffset, formals, label} = formals
	
	fun name {localOffset, formals, label} = label
	
	(* Aloca una variable local en el frame pasado como argumento *)
	fun allocLocal {localOffset, formals, label} escapes =
		if escapes then
		    (localOffset := !localOffset + incrLocal; InFrame(!localOffset) )
		else InReg (newtemp())
									 
    fun exp (InReg r) _ = TEMP r
      | exp (InFrame offset) stackAddr = MEM (BINOP (PLUS, CONST offset, stackAddr))

	(* Genera un fragmento para los string *)
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun getFrameLabel (frame:frame) = #label(frame)
	
	fun procEntryExit1 (body, frame) = body
	
    (*!!! OJO VER EL jump=SOME[] !!! pa' que mierda está?*)
	fun procEntryExit2 (frame, body) =
	    body @ [TigerAssem.OPER {assem="", src=[R0, RA, SP]@calleesaves, dst=[], jump=SOME[]}]
	    
(*	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end

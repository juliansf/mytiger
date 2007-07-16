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
	
	val RV = namedtemp("RV")
	val FP = namedtemp("FP")
	val SP = namedtemp("SP")
    val R0 = namedtemp("R0")
    val r8 = namedtemp("R8")
    val r9 = namedtemp("R9")
    val r10 = namedtemp("R10")
    val r11 = namedtemp("R11")
    val r12 = namedtemp("R12")
    val r13 = namedtemp("R13")

	val registers = ["RV","FP","SP","R0"]
	val tempMap = tabNueva()

	val wordSize = 8
	val prologSize = wordSize * 1
	val incrLocal = ~wordSize

    val argregs = [r8, r9, r10, r11, r12, r13]

    (* !!!! ARREGLAR: armar la lista de los caller saves, r13 es fruta  !!!! *)
    val calldefs = [r13]

	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

	fun newFrame (name, formals) =
		let
			val argsoffset = ref 0
			fun processFormals (arg, argList) = 
				case arg of true => (argsoffset := !argsoffset - incrLocal;
														 InFrame(!argsoffset) :: argList) 
										|false => InReg (newtemp()) :: argList
        in
		    (*true::formals es para dejar como 1er arg al static link*)											
			{localOffset = ref 0, formals = List.foldl processFormals [] (true::formals), label = name}		
		end
			
	fun formals {localOffset, formals, label} = formals
	
	fun name {localOffset, formals, label} = label
	
	fun allocLocal {localOffset, formals, label} escapes =
		if escapes
		then (localOffset := !localOffset + incrLocal; InFrame(!localOffset) )
		else InReg (newtemp())
									 
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun sl_access ~1 = TEMP FP
		| sl_access 0 = MEM (BINOP (PLUS, CONST prologSize, TEMP FP))
		| sl_access n = MEM (BINOP (PLUS, CONST prologSize, sl_access (n-1)))
	
	fun var_access (InReg t, _) = TEMP t
		| var_access (InFrame off, n) =
				let 
					fun aux 0 = TEMP FP
					  | aux n = MEM (BINOP (PLUS, CONST prologSize, aux (n-1)))
				in MEM (BINOP (PLUS, CONST off, aux n)) end
	
	fun getFrameLabel (frame:frame) = #label(frame)
	
	fun procEntryExit1 (body, frame) = body
(*	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.inst list
	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end

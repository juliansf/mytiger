structure TigerFrame :> TigerFrame =
struct
	open TigerTemp
	open tigertab
	open TigerTree
	type register = string
	datatype access = InFrame of int | InReg of TigerTemp.temp
	type frame = {localoffset: int ref, formals: access list, label: TigerTemp.label}
	
	val RV = namedtemp("RV")
	val FP = namedtemp("FP")
	val SP = namedtemp("SP")
	val registers = ["RV","FP","SP"]
	val tempMap = tabNueva()
	val wordSize = 8
	val incrLocal = ~wordSize
	fun externalCall (name, params) = CALL (NAME (namedlabel(name)), params)

	fun newFrame (name, formals) =
		let
			val argsoffset = ref 0
			fun processFormals (arg, argList) = 
				case arg of true => (argsoffset := !argsoffset - incrLocal;
														 InFrame(!argsoffset) :: argList) 
										|false => InReg (newtemp()) :: argList
		in																																								
			{localoffset = ref 0, formals = List.foldl processFormals [] (true::formals), label = name}		(*true::formals es para dejar como 1er arg al static link*)
		end
			
	fun formals {localoffset, formals, label} = formals
	
	fun name {localoffset, formals, label} = label
	
	fun allocLocal {localoffset, formals, label} escapes =
		case escapes of true => (localoffset := !localoffset + incrLocal;
														 InFrame(!localoffset) )
									| false => InReg(newtemp())
									 
	fun string label s = labelname(label) ^ ": .ascii \"" ^ s ^ "\"\n"
	
	fun procEntryExit1 (frame, body) = body
(*	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.inst list
	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
								| STRING of TigerTemp.label * string
end
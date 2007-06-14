signature TigerFrame =
sig
	type register
	type frame
	type access
	
	val RV : TigerTemp.temp
	val FP : TigerTemp.temp
	val SP : TigerTemp.temp
	val registers : register list
	val tempMap : (register, TigerTemp.temp) tigertab.Tabla
	val wordSize : int
	val externalCall : string * TigerTree.exp list -> TigerTree.exp
	val newFrame : (TigerTemp.label * bool list) -> frame
	val formals : frame -> access list
	val name : frame -> TigerTemp.label
	val allocLocal : frame -> bool -> access
	val string : TigerTemp.label -> string -> string
	
	val procEntryExit1 : frame * TigerTree.stm -> TigerTree.stm
(*	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.inst list
	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
								| STRING of TigerTemp.label * string
end
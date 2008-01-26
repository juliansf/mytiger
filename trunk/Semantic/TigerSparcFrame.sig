signature TigerSparcFrame =
sig
	type register
	type frame
	type access
	
	datatype frag = PROC of { body : TigerTree.stm, frame : frame }
								| STRING of TigerTemp.label * string
	
	val RV : TigerTemp.temp
	val FP : TigerTemp.temp
	val SP : TigerTemp.temp
	val R0 : TigerTemp.temp
	val specialregs : TigerTemp.temp list
	val argregs : TigerTemp.temp list
	val calleesaves : TigerTemp.temp list
	val callersaves : TigerTemp.temp list
	val calldefs : TigerTemp.temp list

	val wordSize : int
	val prologSize : int
	val slOffset : int
	val externalCall : string * TigerTree.exp list -> TigerTree.exp
	val newFrame : (TigerTemp.label * bool list) -> frame
	val formals : frame -> access list
	val name : frame -> TigerTemp.label
	val allocLocal : frame -> bool -> access
	val string : TigerTemp.label -> string -> string
	val getFrameLabel : frame -> TigerTemp.label
	val exp : access -> TigerTree.exp -> TigerTree.exp
	
	val procEntryExit1 : TigerTree.stm * frame -> TigerTree.stm
	val procEntryExit2 : frame * TigerAssem.instr list -> TigerAssem.instr list
(*	val procEntryExit3 : frame * TigerAssem.instr list -> 
												{ prolog : string, body : TigerAssem.instr list, epiloge : string }*)
end

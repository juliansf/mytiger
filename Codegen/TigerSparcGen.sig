signature TigerSparcGen =
sig
    val codegen : TigerFrame.frame -> TigerTree.stm -> TigerAssem.instr list
	val literals : TigerCanon.frag list -> string
end

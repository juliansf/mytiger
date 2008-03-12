signature TigerColor =
sig
	val color: TigerAssem.instr list * TigerFrame.frame -> { prolog : string, body : TigerAssem.instr list, epilogue : string }
end

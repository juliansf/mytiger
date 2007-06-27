signature TigerCanon =
sig
	datatype frag = LITERAL of TigerTemp.label * string
								| FUNC of { body:TigerTree.stm list, frame: TigerFrame.frame }
	
	val canonize : TigerFrame.frag list -> frag list
end
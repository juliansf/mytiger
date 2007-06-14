signature TigerTemp =
sig
	type temp
	type label
	val newlabel : unit -> label
	val newtemp : unit -> temp
	val namedtemp : string -> temp
	val namedlabel : string -> label
	val labelname : label -> string
	val tempname : temp -> string
end
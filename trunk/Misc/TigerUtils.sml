structure TigerUtils =
struct
	open TigerTypes
	open TigerError

	fun isInt x = case x of INT _ => true | _ => false
	
	fun showtype ty =
		case ty of
				INT RW => "int rw"
			| INT RO => "int ro"
			| STRING => "string"
			| RECORD (fl,u) => 
					"record { " ^ List.foldl ((fn ((n,t), y) => y ^ n ^ ":" ^ showtype t ^ ", ")) "" fl ^ "\b\b }"
			| ARRAY (t,u) => "array of " ^ showtype t
			| NIL => "nil"
			| UNIT => "unit"
			| NAME (name, ref NONE) => "name('" ^ name ^ "', none)"
			| NAME (name, ref (SOME t)) => "name('" ^ name ^ "', " ^ showtype t ^ ")" 
	
	fun weakCompTypes ( a, b ) =
		case ( a, b ) of
			( NIL, NIL ) => false
		| ( UNIT, UNIT ) => true
		|	( INT _, INT _ ) => true
		| ( STRING, STRING ) => true
		| ( RECORD _, NIL ) => true
		| ( NIL, RECORD _ ) => true
		| ( RECORD (_,uniq1), RECORD (_,uniq2) ) => uniq1 = uniq2
		| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => uniq1 = uniq2
		| _ => false
end
structure TigerTypes =
struct
	datatype state = RO | RW
	datatype ty =
		INT of state
	| STRING
	| RECORD of (string * ty) list * unit ref
	| ARRAY of ty * unit ref
	| NIL
	| UNIT
	| NAME of string * ty option ref
end
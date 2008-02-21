signature TigerStack =
sig
	type 'a stack
	exception emptyStack

	val newStack  : unit -> 'a stack
	val pushStack : 'a -> 'a stack -> unit
	val popStack  :	'a stack -> 'a
	val topStack  : 'a stack -> 'a
	val isEmptyStack : 'a stack -> bool
	val stackToList : 'a stack -> 'a list
	val stackPP : ('a -> string) -> 'a stack -> string

end


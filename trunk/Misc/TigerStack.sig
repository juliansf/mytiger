signature TigerStack =
sig
	type 'a stack

	val newStack  : unit -> 'a stack
	val pushStack : 'a -> 'a stack -> unit
	val popStack  :	'a stack -> 'a
	val topStack  : 'a stack -> 'a

end


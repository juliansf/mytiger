signature TigerMap =
sig
    type ('key, 'a) mapT

	val newMap       : ('key * 'key -> order) -> ('key, 'a) mapT
	val insertMap    : ('key, 'a) mapT * 'key * 'a -> unit
	val findMap      : ('key, 'a) mapT * 'key -> 'a
	val peekMap      : ('key, 'a) mapT * 'key -> 'a option
	val listItemsMap : ('key, 'a) mapT -> ('key * 'a) list
	val transformMap : ('a -> 'a) -> ('key,'a) mapT -> unit
	val appMap       : ('key * 'a -> unit) -> ('key,'a) mapT -> unit
end

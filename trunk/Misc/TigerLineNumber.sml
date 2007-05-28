structure TigerLineNumber =
struct
	val line = ref 1
	fun incLine () = line := !line + 1
	fun Line () = !line
end
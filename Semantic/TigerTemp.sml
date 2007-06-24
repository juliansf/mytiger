structure TigerTemp :> TigerTemp =
struct
	type label = string
	type temp = string
	
	local
		val lcount = ref 0
		val tcount = ref 0
	in
		fun newname pre count =
			let
				val n = !count 
			in
				count := !count + 1;
				pre ^ Int.toString n
			end
		
		fun newlabel () = newname "L" lcount
		fun newtemp () = newname "T" tcount
		fun namedtemp s = s
		fun namedlabel s = s
		fun namedlabel2 s = s ^ "_" ^ newlabel()
		fun labelname s = s
		fun tempname t = t
	end
end

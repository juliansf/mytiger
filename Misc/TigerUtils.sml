structure TigerUtils =
struct
	open TigerTypes
	open TigerError

	fun isInt x = case x of INT _ => true | _ => false
	
	fun showtype ty = showtype' ty 2
	and showtype' ty deep=
		case ty of
				INT RW => "int rw"
			| INT RO => "int ro"
			| STRING => "string"
			| RECORD (fl,u) => 
					"record { " ^ List.foldl ((fn ((n,t), y) => y ^ n ^ ":" ^ showtype' t deep ^ ", ")) "" fl ^ "\b\b }"
			| ARRAY (t,u) => "array of " ^ showtype' t deep
			| NIL => "nil"
			| UNIT => "unit"
			| NAME (name, ref NONE) =>  "name('" ^ name ^ "', none)"
			| NAME (name, ref (SOME t)) => 
					if 0 < deep then "name('" ^ name ^ "', " ^ showtype' t (deep-1) ^ ")"
					else "#"
	
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
		| ( NAME (n,tyr), b ) => (
			case !tyr of
				SOME (r as RECORD _) => weakCompTypes (r, b)
			| SOME (a as ARRAY _) => weakCompTypes (a, b)
			| _ => raise InternalError "comparando tipo NAME que tiene (ref NONE) en Utils.weakCompTypes!")
		| ( a, NAME (n,tyr)) => (
			case !tyr of
				SOME (r as RECORD _) => weakCompTypes (a, r)
			| SOME (b as ARRAY _) => weakCompTypes (a, b)
			| _ => raise InternalError "comparando tipo NAME que tiene (ref NONE) en Utils.weakCompTypes!")
		| _ => false
		
	
	fun listRemoveItem f [] = []
	 |	listRemoveItem f (y::ys) = if f y then ys else y :: listRemoveItem f ys

	fun cyclesort graph =
	  let 
	  	infix mem
			fun x mem []  =  false
			  | x mem (y::l)  =  (x=y) orelse (x mem l)
			
			fun nexts (a, []) = []
			  | nexts (a, (x,y)::pairs) =
			      if a=x then  y :: nexts(a,pairs)
			             else       nexts(a,pairs)
			  
		  fun newvisit (x, (visited,cys)) = (x::visited, cys)
		 	
		  fun sort ([], path, (visited,cys)) = (visited, cys)
		    | sort (x::xs, path, (visited,cys)) =
		        sort(xs, path, 
		           if x mem path   then  (visited, x::cys)
		           else if x mem visited then (visited, cys)
		           else newvisit(x, sort(nexts(x,graph),
		                                 x::path, (visited,cys))))
		  val (xs,_) = ListPair.unzip graph
	  in sort(xs, [], ([],[])) end;
end
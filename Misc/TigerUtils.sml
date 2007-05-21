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
			| NAME (name, ref NONE) =>  "name('" ^ name ^ "', none)"
			| NAME (name, ref (SOME t)) => "#"(*if !deep < 3 then ( deep := !deep+1; "name('" ^ name ^ "', " ^ showtype t ^ ")") 
																		 else "..."*)
	
	fun weakCompTypes ( a, b ) = (print ("CompTypes: " ^ showtype a ^ " :: " ^ showtype b ^ "\n");
		case ( a, b ) of
			( NIL, NIL ) => false
		| ( UNIT, UNIT ) => true
		|	( INT _, INT _ ) => true
		| ( STRING, STRING ) => true
		| ( RECORD _, NIL ) => true
		| ( NIL, RECORD _ ) => true
		| ( RECORD (_,uniq1), RECORD (_,uniq2) ) => uniq1 = uniq2
		| ( ARRAY (_,uniq1), ARRAY (_,uniq2) ) => uniq1 = uniq2
		| ( NAME _, NAME _) => Error (ErrorInternalError "10", 0)
		| ( NAME (n,tyr), b ) => (
			case !tyr of
				SOME (r as RECORD _) => weakCompTypes (r, b)
			| SOME (a as ARRAY _) => weakCompTypes (a, b)
			| _ => raise InternalError "comparando tipo NAME que tiene (ref NONE)!")
		| ( a, NAME (n,tyr)) => (
			case !tyr of
				SOME (r as RECORD _) => weakCompTypes (a, r)
			| SOME (b as ARRAY _) => weakCompTypes (a, b)
			| _ => Error (ErrorInternalError "12", 0))
		| _ => false)
		
	
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
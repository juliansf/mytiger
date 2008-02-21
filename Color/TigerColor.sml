structure TigerColor:>TigerColor =
struct
	open TigerSet
	open TigerMap
	open TigerStack
	open TigerTemp
	open TigerFrame
	open TigerFlow
	open TigerAssem
	open TigerError
	
fun color (linstr) =
let
	val initial = newSet comparetemps
	val simplifyWorklist = newSet comparetemps	
	val freezeWorklist = newSet comparetemps
	val spillWorklist = newSet comparetemps	
	val spilledWorklist = newSet comparetemps
	val coalescedNodes = newSet comparetemps
	val selectStack = newStack ()

	val coalescedMoves = newSet Int.compare
	val constrainedMoves = newSet Int.compare
	val frozenMoves = newSet Int.compare
	val worklistMoves = newSet Int.compare
	val activeMoves = newSet Int.compare			

	val adjList = newMap comparetemps
	val degree = newMap comparetemps
	val moveList = newMap comparetemps

	val alias = newMap comparetemps
	val color = newMap comparetemps

	val k = List.length (TigerFrame.registerlist)
	
	val single = singletonSet comparetemps
		
	val adjSet = let
					fun compareEdges ((t1,t2),(t3,t4)) = case (comparetemps (t1,t3), comparetemps(t2,t4)) of
													       (EQUAL,EQUAL) => EQUAL
													     | (EQUAL,x) => x
													     | (x,_) => x
				 in
				 	newSet compareEdges
				 end

	(*Busca la clave key en map, si la encuetra devuelve su valor asociado sino devuelve e*)
	fun tryPeekMapOrNew map key e = case (peekMap (map, key)) of
				        	              SOME s => s
				        	            | NONE => e
				        	            
	fun AddEdge (u,v) = if memberSet(adjSet,(u,v)) = false andalso u <> v then
							(addElemSet(adjSet, (u,v));
							 addElemSet(adjSet, (v,u));
							if not(List.exists (fn x => x = u) precolored) then
								(insertMap (adjList, u,
									unionSet (tryPeekMapOrNew adjList u (newSet comparetemps),
											  singletonSet comparetemps v));
								 insertMap (degree, u, (tryPeekMapOrNew degree u 0) + 1))
							else ();
							if not(List.exists (fn x => x = v) precolored) then
								(insertMap (adjList, v,
									unionSet (tryPeekMapOrNew adjList v (newSet comparetemps),
				        	              	  singletonSet comparetemps u));
				        	    insertMap (degree, v, (tryPeekMapOrNew degree v 0) + 1))
							else ())
						 else ()
 
(* Función para debug que imprime las distintas estructuras de datos *)
    fun printLists() = (print ("adjSet = \n"^(setPP (fn (x,y) => "("^(TigerTemp.tempname x)^","
    															  ^(TigerTemp.tempname y)^")") adjSet));
    					print "\n\nadjList = \n";
						mapPP tempname (setPP tempname) adjList;
						print "\nmoveList = \n";
						mapPP tempname (setPP Int.toString) moveList;
						print ("\nworklistMoves = "^ (setPP Int.toString worklistMoves));
						print "\ndegree = \n";
						mapPP tempname Int.toString degree;
						print "\n"
						)    
 
 	fun printWorklists() = (
 		print ("\nsimplifyWorklist =\n" ^ setPP tempname simplifyWorklist);
 		print ("\nfreezeWorklist =\n" ^ setPP tempname freezeWorklist);
 		print ("\nspillWorklist =\n" ^ setPP tempname spillWorklist);
 		print ("\nspilledWorklist =\n" ^ setPP tempname spilledWorklist)
	)
 
 	fun Build () =
	let
		val liveOut = liveness linstr
		val live = newSet comparetemps
		
		(*Esta función se aplica a la lista de instrucciones indexada y en orden inverso*)
		fun aux (i,inst) =
					(* En el caso que sea un salto actualizamos el conjunto de temporales vivos
					   utilizando la información del análisis de liveness *)
					(if isJump inst then
						(setEmptySet live;
						unionInSet (live, findMap (liveOut,i)))
					else ();
					if isMove inst then
				            (differenceInSet (live, uses inst);
				        	 forallSet (fn n => insertMap (moveList, n,
				        	                      unionSet (tryPeekMapOrNew moveList n (newSet Int.compare),
															singletonSet Int.compare i))
												) (unionSet (uses inst,defs inst));
							addElemSet (worklistMoves,i))
				    else ();			    
				    unionInSet (live, defs inst);
				    forallSet (fn d => forallSet (fn l => AddEdge(l,d)) live) (defs inst);
				    differenceInSet (live, defs inst);
				    unionInSet (live, uses inst))
	in
		List.app aux (List.rev (ListPair.zip (TigerUtils.intRange 0 ((List.length linstr) - 1),linstr)))
	end
	
	fun NodeMoves n = intersectionSet (tryPeekMapOrNew moveList n (newSet Int.compare),unionSet(activeMoves, worklistMoves))
	
	fun MoveRelated n = not (isEmptySet (NodeMoves n))
	
	fun MakeWorklist () =
	let
		fun aux n = (
		    differenceInSet(initial,singletonSet comparetemps n);
			if findMap(degree,n) >= k then
				addElemSet(spillWorklist, n)
			else if MoveRelated n then
				addElemSet(freezeWorklist, n)
			else
				addElemSet(simplifyWorklist,n)
		 ) handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:MakeWorklist()\n",0);

        (* Se utiliza como temporal para borrar los registros precoloreados de initial *)
        (* Es necesario borrarlos ya que solo debo colorear los temporales *)
		val t = newSet comparetemps
	in
		List.app (fn i=> unionInSet(initial,unionSet(uses i,defs i))) linstr;
		addListSet(t, precolored);
		differenceInSet(initial,t);
		forallSet aux initial
	end
	
	fun Adjacent n =
	    let val t = newSet comparetemps
	    in
	    	addListSet(t,stackToList selectStack);
    	(* !!! Acá se puede mejorar, porque si tryPeekOrNew devuelve el newSet, no tiene sentido hacer el diff *)
	    	differenceSet(tryPeekMapOrNew adjList n (newSet comparetemps), unionSet(coalescedNodes,t))
	    end

	fun EnableMoves nodes =
	    forallSet (fn n =>
	    	forallSet (fn m => if memberSet(activeMoves,m) then
	    					       (differenceInSet(activeMoves, singletonSet Int.compare m);
	    					       unionInSet(worklistMoves, singletonSet Int.compare m))
	    					   else ()) (NodeMoves n)) nodes 
	
	fun DecrementDegree m =
	    let val d = findMap(degree,m)
		in
			insertMap(degree,m,d-1);
			if d = k then (
				EnableMoves(unionSet(single m, Adjacent m));
				differenceInSet(spillWorklist, single m);
				if MoveRelated m then
					unionInSet(freezeWorklist, single m)
				else
					unionInSet(simplifyWorklist, single m))
			else ()
		end
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:DecrementDegree(m)\n",0);


	fun Simplify () =
	    let val n = elemFromSet simplifyWorklist
	    in
	    	differenceInSet(simplifyWorklist, singletonSet comparetemps n);
	    	pushStack n selectStack;
	    	forallSet (fn m => DecrementDegree m) (Adjacent n)
	    end
    
	
in 
	Build();
	MakeWorklist();
    printLists();
    printWorklists();
    Simplify()
end

end

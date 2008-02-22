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
	val degree = 
		let
			val t = newMap comparetemps
			val max = valOf(Int.maxInt)
		in
			List.app (fn x => insertMap(t,x,max)) precolored;
			t		
		end

	
	val moveList = newMap comparetemps

	val alias = newMap comparetemps
	val color = newMap comparetemps

	val k = List.length (TigerFrame.registerlist)
	
	val singleTemp = singletonSet comparetemps
	val singleInt = singletonSet Int.compare

	val precolored = let val t = newSet comparetemps
					 in
					 	addListSet(t, TigerFrame.precolored);
					 	t
					 end
					 			
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
				        	            
	fun AddEdge (u,v) = if not(memberSet(adjSet,(u,v))) andalso not(equalTemp(u,v)) then
							(addElemSet(adjSet, (u,v));
							 addElemSet(adjSet, (v,u));
							if not(memberSet(precolored, u)) then
								(insertMap (adjList, u,
									unionSet (tryPeekMapOrNew adjList u (newSet comparetemps), singleTemp v));
								 insertMap (degree, u, (tryPeekMapOrNew degree u 0) + 1))
							else ();
							if not(memberSet(precolored,v)) then
								(insertMap (adjList, v,
									unionSet (tryPeekMapOrNew adjList v (newSet comparetemps), singleTemp u));
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
													singleInt i))
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
	
	fun NodeMoves n = 
	    intersectionSet (tryPeekMapOrNew moveList n (newSet Int.compare),unionSet(activeMoves, worklistMoves))
	
	fun MoveRelated n = not (isEmptySet (NodeMoves n))
	
	fun MakeWorklist () =
	let
		fun aux n = (
		    differenceInSet(initial,singleTemp n);
			if findMap(degree,n) >= k then
				addElemSet(spillWorklist, n)
			else if MoveRelated n then
				addElemSet(freezeWorklist, n)
			else
				addElemSet(simplifyWorklist,n)
		 ) handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:MakeWorklist()\n",0);

	in
		List.app (fn i=> unionInSet(initial,unionSet(uses i,defs i))) linstr;
		differenceInSet(initial,precolored);
		forallSet aux initial
	end
	
	fun Adjacent n =
	    let val t = newSet comparetemps
	    in
	    	print (tempname n);
	    	addListSet(t,stackToList selectStack);
    	(* !!! Acá se puede mejorar, porque si tryPeekOrNew devuelve el newSet, no tiene sentido hacer el diff *)
	    	differenceSet(tryPeekMapOrNew adjList n (newSet comparetemps), unionSet(coalescedNodes,t))
	    end

	fun EnableMoves nodes =
	    forallSet (fn n =>
	    	forallSet (fn m => if memberSet(activeMoves,m) then
	    					       (differenceInSet(activeMoves, singleInt m);
	    					       unionInSet(worklistMoves, singleInt m))
	    					   else ()) (NodeMoves n)) nodes 
	
	fun DecrementDegree m =
	    let val d = findMap(degree,m)
		in		
			insertMap(degree,m,d-1);
			if d = k then (
				EnableMoves(unionSet(singleTemp m, Adjacent m));
				differenceInSet(spillWorklist, singleTemp m);
				if MoveRelated m then
					unionInSet(freezeWorklist, singleTemp m)
				else
					unionInSet(simplifyWorklist, singleTemp m))
			else ()
		end
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:DecrementDegree(m)\n",0)


	fun Simplify () =
	    let val n = elemFromSet simplifyWorklist
	    in
	    	differenceInSet(simplifyWorklist, singleTemp n);
	    	pushStack n selectStack;
	    	forallSet DecrementDegree (Adjacent n)
	    end
    
	fun AddWorkList u =
		if not(memberSet(precolored,u)) andalso not(MoveRelated u) andalso findMap(degree,u) < k then
		    ( differenceInSet(freezeWorklist, singleTemp u);
		      unionInSet(simplifyWorklist, singleTemp u))
		else ()
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:AddWorkList(n)\n",0)

	fun OK(t,r) =
		if findMap(degree,t) < k orelse memberSet(precolored,t) orelse memberSet(adjSet,(t,r)) then	true
		else false
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:OK(t,r)\n",0)

	fun Conservative nodes =
		let val k' = ref 0
		in
			forallSet (fn n => if findMap(degree,n) >= k then k' := !k' + 1 else ()) nodes;
			if !k' < k then true else false
		end
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:Conservative nodes\n",0)
		
	fun GetAlias n = 
		if memberSet(coalescedNodes,n) then
			GetAlias(findMap(alias,n))
	    else n
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:GetAlias(n)\n",0)

	fun Combine(u,v) =
	   (if memberSet(freezeWorklist,v) then
		    differenceInSet(freezeWorklist, singleTemp v)
		else differenceInSet(spillWorklist, singleTemp v);
		unionInSet(coalescedNodes, singleTemp v);
		insertMap(alias,v,u);
		insertMap(moveList,u,unionSet(findMap(moveList,u),findMap(moveList,v)));
		EnableMoves (singleTemp v);
		forallSet (fn t => (AddEdge(t,u);DecrementDegree t)) (Adjacent v);
		if findMap(degree,u) >= k andalso memberSet(freezeWorklist,u) then
			(differenceInSet(freezeWorklist, singleTemp u);
			 unionInSet(spillWorklist, singleTemp u))
		else ())
		
    fun Coalesce () =
        let
        	val m = elemFromSet worklistMoves
        	val ins = List.nth (linstr, m)
        	val x = GetAlias (elemFromSet (uses ins))
        	val y = GetAlias (elemFromSet (defs ins))
        	val (u,v) = if memberSet(precolored, y) then (y,x) else (x,y)
        	
        	fun forallOK(u,v) = 
        		let	val flag = ref true
        		in
        			forallSet (fn t => case (!flag, OK(t,u)) of
        							        (true, false) => flag := false
        							       | _ => ()) (Adjacent v);
        			!flag
        		end
        	
        in
        	differenceInSet(worklistMoves, singleInt m);
        	if equalTemp(u,v) then
        		(unionInSet(coalescedMoves, singleInt m);
        		 AddWorkList u)
        	else if memberSet(precolored,v) orelse memberSet(adjSet,(u,v)) then
        		(unionInSet(constrainedMoves,singleInt m);
        		 AddWorkList u;
        		 AddWorkList v)
        	else if (memberSet(precolored,u) andalso forallOK(u,v)) orelse 
        			(not(memberSet(precolored,u)) andalso Conservative(unionSet(Adjacent u, Adjacent v))) then
        			
        			(unionInSet(coalescedMoves, singleInt m);
        			 Combine(u,v);
        			 AddWorkList(u))
        	else
        		unionInSet(activeMoves, singleInt m)
        end

in 
	Build();
	MakeWorklist();
    printLists();
    printWorklists();
    Simplify()
end

end

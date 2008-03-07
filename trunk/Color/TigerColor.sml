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
	
fun color (li,fr) =
let
	val linstr = ref li
	val frame = ref fr

	val initial = newSet comparetemps
	val simplifyWorklist = newSet comparetemps	
	val freezeWorklist = newSet comparetemps
	val spillWorklist = newSet comparetemps	
	val spilledNodes = newSet comparetemps
	val coalescedNodes = newSet comparetemps
	val coloredNodes = newSet comparetemps
	val selectStack = newStack ()

	val coalescedMoves = newSet Int.compare
	val constrainedMoves = newSet Int.compare
	val frozenMoves = newSet Int.compare
	val worklistMoves = newSet Int.compare
	val activeMoves = newSet Int.compare			

	val adjList = newMap comparetemps

	(* Creo el mapa de grados y asigno a los nodos precoloreados un grado "infinito" *)
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

	(* Creo el mapa de colores y asigno a los nodos precoloreados sus respectivos colores *)
	val color =
		let 
			val t =	newMap comparetemps
			val n = (List.length TigerFrame.precolored) - 1
		in
			List.app (fn (v,k) => insertMap(t,k,v)) (ListPair.zip (TigerUtils.intRange 0 n, TigerFrame.precolored));
			t
		end

	val k = List.length (TigerFrame.registerlist)
	
	val singleTemp = singletonSet comparetemps
	val singleInt = singletonSet Int.compare

	val precolored =
		let val t = newSet comparetemps
		in
			addListSet(t, TigerFrame.precolored); t
		end
					 			
	val adjSet =
		let
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
    fun printIGraph() = (
			     		print ("\n-------- IGraph ---------\n");
    					print ("adjSet = \n"^(setPP (fn (x,y) => "("^(TigerTemp.tempname x)^","
    															  ^(TigerTemp.tempname y)^")") adjSet));
    					print "\n\nadjList = \n";
						mapPP tempname (setPP tempname) adjList;
						print "\nmoveList = \n";
						mapPP tempname (setPP Int.toString) moveList;
						print ("\nworklistMoves = "^ (setPP Int.toString worklistMoves));
						print "\ndegree = \n";
						mapPP tempname Int.toString degree;
				 		print ("\n-------------------------\n")
						)    

	fun printMovesStructs() =(
   		print ("\n-------- Moves ---------\n");
		print("\nworklistMoves:\n" ^ (setPP Int.toString worklistMoves));
		print("\ncoalescedMoves:\n" ^ (setPP Int.toString coalescedMoves));
		print("\nconstrainedMoves:\n" ^ (setPP Int.toString constrainedMoves));
		print("\nfrozenMoves:\n" ^ (setPP Int.toString frozenMoves ));
		print("\nactiveMoves:\n" ^ (setPP Int.toString activeMoves));
		print("\nMoveList:\n") ; (mapPP tempname (setPP Int.toString) moveList);
		print ("\n------------------------\n")
	)
	
 	fun printWorklists() = (
 		print ("\n-------- Worklists ---------\n");
 		print ("\nsimplifyWorklist =\n" ^ setPP tempname simplifyWorklist);
 		print ("\nfreezeWorklist =\n" ^ setPP tempname freezeWorklist);
 		print ("\nspillWorklist =\n" ^ setPP tempname spillWorklist);
 		print ("\nspilledNodes =\n" ^ setPP tempname spilledNodes);
 		print ("\n-------- Worklists ---------\n")
	)
 
 	fun Build () =
	let
		val liveOut = liveness (!linstr)
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
		List.app aux (List.rev (ListPair.zip (TigerUtils.intRange 0 ((List.length (!linstr)) - 1),(!linstr))))
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
		List.app (fn i=> unionInSet(initial,unionSet(uses i,defs i))) (!linstr);
		differenceInSet(initial,precolored);
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
			(!k' < k)	
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
		(* puede que moveList[u] no exista, por lo que habría que agregar un tryPeekMapOrNew *)
		insertMap(moveList,u,unionSet(findMap(moveList,u),findMap(moveList,v)));
		EnableMoves (singleTemp v);
		forallSet (fn t => (AddEdge(t,u);DecrementDegree t)) (Adjacent v);
		if findMap(degree,u) >= k andalso memberSet(freezeWorklist,u) then
			(differenceInSet(freezeWorklist, singleTemp u);
			 unionInSet(spillWorklist, singleTemp u))
		else ())
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:Combine(u,v)\n",0)
		
    fun Coalesce () =
        let
        	val m = elemFromSet worklistMoves
        	val ins = List.nth ((!linstr), m)
			val (x,y) = case ins of
						    MOVE {src=s, dst=d,...} => (GetAlias(s),GetAlias(d))
						  | _ => Error (ErrorInternalError "Error interno en TigerColor.sml:Coalesce()\n",0)

        	val (u,v) = if memberSet(precolored, y) then (y,x) else (x,y)
        	
        	fun forallOK(u,v) =	List.all (fn t => OK(t,u)) (listItemSet (Adjacent v))        	
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
	
	fun FreezeMoves u =
		forallSet (fn m => 
			let
			    val ins = List.nth ((!linstr), m)
			    val (x,y) = case ins of
				    		    MOVE {src=s, dst=d,...} => (s,d)
					    	  | _ => Error (ErrorInternalError "Error interno en TigerColor.sml:FreezeMoves(u)\n",0)
				val v = if equalTemp(GetAlias y, GetAlias u) then GetAlias x else GetAlias y
			in
				differenceInSet(activeMoves, singleInt m);
				unionInSet(frozenMoves, singleInt m);
				if isEmptySet(NodeMoves v) andalso findMap(degree,v) < k then
					(differenceInSet(freezeWorklist, singleTemp v);
					 unionInSet(simplifyWorklist, singleTemp v))
				else ()
			end
		) (NodeMoves u)
		handle NotFound => Error (ErrorInternalError "Error interno en TigerColor.sml:FreezeMoves u\n",0)

	fun Freeze() =
		let val u = elemFromSet freezeWorklist
		in
			differenceInSet(freezeWorklist, singleTemp u);
			unionInSet(simplifyWorklist, singleTemp u);
			FreezeMoves u
		end
	
	fun SelectSpill () =
		(* Debería implementar alguna heurística *)
		let val m = elemFromSet spillWorklist
		in
			differenceInSet(spillWorklist, singleTemp m);
			unionInSet(simplifyWorklist, singleTemp m);
			FreezeMoves m
		end
		
	fun AssignColors () =
	    (while not(isEmptyStack selectStack) do
	        let
	        	val n = popStack selectStack
	        	val okColors = let val t = newSet Int.compare
	        				   in
	        				       addListSet(t,TigerUtils.intRange 0 (k-1)); t
	        				   end
	        in
	        	forallSet (fn w => if memberSet(unionSet(coloredNodes,precolored) ,GetAlias w) then
	        					       differenceInSet(okColors, singleInt(findMap(color,GetAlias w)))
	        					   else ()) (findMap (adjList,n));
	        	if isEmptySet(okColors) then
	        		unionInSet(spilledNodes, singleTemp n)
	        	else
	        	   (unionInSet(coloredNodes, singleTemp n);
	        	    insertMap(color,n,elemFromSet okColors))
	        end;
	        forallSet (fn n => insertMap(color,n,findMap(color,GetAlias n))) coalescedNodes)
	
	fun RewriteProgram () =
		let
			val newTemps = newSet comparetemps

			fun st n = if n<0 then "-"^makestring(~n) else makestring(n)
			
			fun genFetch nt off = OPER {assem="ld [`s0+"^st(off+stackBias)^"], `d0\n" ,src=[FP], dst=[nt], jump=NONE}
			
			fun genStore nt off = OPER {assem="st `s1, [`s0+"^st(off+stackBias)^"]\n" ,src=[FP,nt], dst=[], jump=NONE}
			
			fun changeDst(OPER {assem,dst,src,jump},t,nt) = 
				OPER {assem=assem,src=src,jump=jump,dst=List.map (fn x => if equalTemp(x,t) then nt else x) dst}
			  | changeDst(MOVE {assem,dst,src},t,nt) = MOVE {assem=assem,dst=nt,src=src}
			  | changeDst _ = Error (ErrorInternalError "Error interno en TigerColor.sml:changeDst() u\n",0)
			
			fun changeSrc(OPER {assem,dst,src,jump},t,nt) = 
				OPER {assem=assem,dst=dst,jump=jump,src=List.map (fn x => if equalTemp(x,t) then nt else x) src}
			  | changeSrc(MOVE {assem,dst,src},t,nt) = MOVE {assem=assem,dst=dst,src=nt}
			  | changeSrc _ = Error (ErrorInternalError "Error interno en TigerColor.sml:changeSrc() u\n",0)
			
			fun updateMoveSets n =
				let 
					fun updatePos l = List.map (fn x => if x >= n then x+1 else x) l
					fun updateSet l = let val l' = listItemSet l
									  in setEmptySet l; addListSet (l,updatePos l');l end
					val worklm = listItemSet worklistMoves
					val coalm = listItemSet coalescedMoves
					val consm = listItemSet constrainedMoves
					val frozm = listItemSet frozenMoves
					val actm = listItemSet activeMoves
					val lm = listItemsMap moveList 
				in
					setEmptySet(worklistMoves); addListSet(worklistMoves,updatePos worklm);
					setEmptySet(coalescedMoves); addListSet(coalescedMoves,updatePos coalm);
					setEmptySet(constrainedMoves); addListSet(constrainedMoves,updatePos consm);
					setEmptySet(frozenMoves); addListSet(frozenMoves,updatePos frozm);
					setEmptySet(activeMoves); addListSet(activeMoves,updatePos actm);
					List.app (fn (k,v) => insertMap(moveList,k,updateSet v)) lm
				end
			
			fun aux v = 
				let
					val off = getAccessOffset (allocLocal (!frame) true)
					
					fun aux2 ((pos,ins),lins) =
						if memberSet(uses ins, v) then
							let
								val new_v = TigerTemp.newtemp()
								val fetch = genFetch new_v off
								val ins' = changeSrc (ins, v, new_v)
								val lins' = List.map (fn (p,i) => (p+1,i)) lins
							in
								updateMoveSets(pos);
								addElemSet(newTemps,new_v);
								(pos,fetch)::(pos+1,ins')::lins'
							end											        	
				   		else if memberSet(defs ins, v) then
							let
								val new_v = TigerTemp.newtemp()
								val store = genStore new_v off
								val ins' = changeDst (ins, v, new_v)
								val lins' = List.map (fn (p,i) => (p+1,i)) lins
							in
								updateMoveSets(pos+1);
								addElemSet(newTemps,new_v);
								(pos,ins')::(pos+1,store)::lins'
							end													   		
				   		else 
				   			(pos,ins)::lins
				in				
					linstr := #2(ListPair.unzip (List.foldr aux2 [] (ListPair.zip (TigerUtils.intRange 0 ((List.length (!linstr)) - 1),(!linstr)))))
				end	
		in
			forallSet aux spilledNodes;
			setEmptySet(spilledNodes);
			setEmptySet(initial);
			unionInSet(initial,coloredNodes);
			unionInSet(initial,coalescedNodes);
			unionInSet(initial,newTemps);
			setEmptySet(coloredNodes);
			setEmptySet(coalescedNodes)
		end

	fun loop() = ( if (not (isEmptySet simplifyWorklist)) then Simplify()
				 else if (not (isEmptySet worklistMoves)) then Coalesce()
				 else if (not (isEmptySet freezeWorklist)) then Freeze()
				 else if (not (isEmptySet spillWorklist)) then SelectSpill()
				 else ();
				 (*printWorklists();
  	 	  		 printMovesStructs();*)
				 if (isEmptySet(simplifyWorklist) andalso isEmptySet(worklistMoves) andalso
				  isEmptySet(freezeWorklist) andalso isEmptySet(spillWorklist)) then ()
				 else loop())

	fun main() = (
		Build();
		MakeWorklist();
(*		printWorklists();
		printMovesStructs();*)
		loop();
		AssignColors();
		if (not (isEmptySet spilledNodes)) then
			(RewriteProgram(); main())
		else ())
in
	main();
	mapPP tempname Int.toString color;
	print "\n";
	List.app (print o (TigerAssem.format TigerTemp.tempname)) (!linstr)
end

end

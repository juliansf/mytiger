structure TigerFlow =
struct
	open TigerError
	open TigerAssem
	open TigerTemp
	open TigerMap
	open TigerSet

(* Calula el conjunto de temporales que utiliza la instrucción ins *)
	fun uses ins =
	    let 
	    	val usesSet = newSet comparetemps
	    in
	    	addListSet(usesSet,
	        	case ins of
	            	OPER {src=sl, ...} => sl
	              | MOVE {src=sl, ...} => [sl]
	              | _ => [] );
	        usesSet
	    end

(* Calula el conjunto de temporales que define la instrucción ins *)
fun defs ins =
    let 
    	val defsSet = newSet comparetemps
    in
    	addListSet(defsSet,
        	case ins of
            	OPER {dst=dl, ...} => dl
              | MOVE {dst=dl, ...} => [dl]
              | _ => [] );
        defsSet
    end 

	fun liveness (instr) =
        let
    		val succmap = newMap Int.compare
            val index = TigerUtils.intRange 0 ((List.length instr) - 1)
            val li = ListPair.zip (index, instr)
            
			(* Prepara los mapas de liveness para todos los nodos *)
            val (liveout,liveout') = (newMap Int.compare, newMap Int.compare)
			val (livein,livein') = (newMap Int.compare, newMap Int.compare)
			
			val _ = List.app (fn i => insertMap (liveout, i, newSet comparetemps)) index
			val _ = List.app (fn i => insertMap (liveout', i, newSet comparetemps)) index
			val _ = List.app (fn i => insertMap (livein, i, newSet comparetemps)) index
			val _ = List.app (fn i => insertMap (livein', i, newSet comparetemps)) index   		    		
    		    		
    		(* Calcula el conjunto de nodos sucesores de n *)
    		fun succ n =
    		    let 
    		        val ins = List.nth(instr, n)
    		        val succSet = newSet Int.compare
    		        
    		        (* Busca el indice correspondiente a label *)
    		        fun searchLabel label =
    		            let fun aux (i, LABEL {lab=l,...}) = l = label
    		            	  | aux _ = false
    		            in
    		            	case (List.find aux li) of
    		            	    SOME (i,inst) => i
    		            	  | NONE => Error (ErrorInternalError "Problema interno en TigerFlow:fun succ n", 0)
    		            end
    		    in
    		    	addListSet(succSet,
    		        case ins of
    		            OPER {jump=SOME labels,...} => List.map searchLabel labels  
    		          | _ => if n = (List.length instr - 1) then [] else [n+1]);
    		        succSet
    		    end
    		
    		(* Calcula los sucesores de todos los nodos del grafo *)
    		fun calc_succ () = List.app (fn n => insertMap (succmap, n, succ n)) index

			fun equalMaps a b = ListPair.all (fn ((_,v1),(_,v2)) => equalSet(v1,v2)) (listItemsMap a, listItemsMap b)

			(* Actualiza los livein y liveouts del nodo n, es un paso del algoritmo de fixpoint *)    		
    		fun updateNode n =
    		    let
    				val ins = List.nth(instr,n)
    				val tmp = newSet comparetemps
    			in
    				insertMap(liveout',n,findMap (liveout,n));
    				insertMap(livein',n,findMap (livein,n));
    				insertMap(livein,n,unionSet (uses(ins),differenceSet(findMap (liveout,n), defs(ins))));
    				forallSet (fn s => unionInSet(tmp,findMap(livein,s))) (findMap (succmap,n));
    				insertMap(liveout,n,tmp)
    			end

    		(* Calcula el liveness de los temporales buscando un punto fijo *)
    		fun fixpoint () = 
   		        (List.app updateNode index;
    		     if (equalMaps livein livein') andalso (equalMaps liveout liveout') then () else fixpoint())
    		       		
    	in
    	  calc_succ ();
    	  fixpoint ();
    	  liveout
    	end
    
    
end

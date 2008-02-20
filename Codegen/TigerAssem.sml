structure TigerAssem :> TigerAssem =
struct
	open Regex
    type reg = string
    type temp = TigerTemp.temp
    type label = TigerTemp.label

    datatype instr = OPER of {assem: string,
                              dst: temp list,
                              src: temp list,
                              jump: label list option}
                   | LABEL of {assem: string,
                               lab: label}   (* <-- acá el tipo podría ser label directamente *)
                   | MOVE of {assem: string,
                              dst: temp,
                              src: temp}

    fun format tempmap inst =
		let
  			fun f l n = tempmap (List.nth (l, valOf(Int.fromString(n))))
  		in
       		case inst of OPER {assem,src,dst,jump} => 
					        replace (regcomp "`d([0-9]+)*" [Extended]) [Tr (f dst, 1)] ( 
					        replace (regcomp "`s([0-9]+)*" [Extended]) [Tr (f src, 1)] assem)
              		   | MOVE {assem, src, dst} =>
					        replace (regcomp "`d([0-9]+)*" [Extended]) [Str (tempmap dst)] ( 
					        replace (regcomp "`s([0-9]+)*" [Extended]) [Str (tempmap src)] assem)
              		   | LABEL {assem, lab} => assem
		end

	fun isJump (OPER {jump=SOME _,...}) = true
	  | isJump _ = false
	
	fun isMove (MOVE _) = true
	  | isMove _ = false
       
end


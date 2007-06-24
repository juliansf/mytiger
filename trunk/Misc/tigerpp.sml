(*
structure tigerpp :> tigerpp = 
struct
*)
open TigerTree
open TigerAbs
open PP
open TigerTemp

fun ppexpr pps e0 = 
	let
		fun ppf{name,escape,typ} =
			(add_string pps ("{name="^name^",");
			add_break pps (0, 1);
			add_string pps " escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps ", typ=";
			add_string pps typ;
			add_string pps "}"; add_break pps (0, 1))
		and ppd(FunctionDec flist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "FunctionDec([";
			List.app
				(fn({name,params,result,body}, _)=>
					(add_string pps ("{name="^name^",");
					add_break pps (0, 0);
					add_string pps "params=[";
					List.app ppf params;
					add_string pps "],"; add_break pps (0, 1);
					add_string pps ("result="^
						(case result of SOME s => s | _ => "NONE"));
					add_break pps (0, 0);
					add_string pps "body="; ppe body;
					add_string pps "}"; add_break pps (0, 0)))
				flist;
			add_string pps "])"; add_break pps (0, 0);
			end_block pps)
		| ppd(VarDec({name, escape, typ, init}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("VarDec{name="^name^",");
			add_break pps (0, 1);
			add_string pps "escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps ","; add_break pps (0, 1);
			add_string pps ("typ="^
				(case typ of SOME s => s | _ => "NONE"));
			add_string pps ","; add_break pps (0, 1);
			add_string pps "init="; ppe init;
			add_string pps "}"; add_break pps (0, 0);
			end_block pps)
		| ppd(TypeDec tlist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "TypeDec([";
			List.app
				(fn({name: symbol, ty: ty}, _)=>
					(add_string pps ("{name="^name^","); add_break pps (0, 1);
					add_string pps "ty=";
					ppt ty; add_string pps "}";
					add_break pps (0, 1)))
				tlist;
			add_string pps "])"; add_break pps (0, 0);
			end_block pps)
		and ppt(NameTy s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("NameTy("^s^")"); add_break pps (0, 0);
			end_block pps)
		| ppt(RecordTy fieldlist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "RecordTy([";
			List.app ppf fieldlist;
			add_string pps "])";
			end_block pps)
		| ppt(ArrayTy s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("ArrayTy("^s^")"); add_break pps (0, 1);
			end_block pps)
		and ppv(SimpleVar s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SimpleVar("; add_break pps (0, 1);
			add_string pps (s^")"); add_break pps (0, 0);
			end_block pps)
		| ppv(FieldVar(v, s)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "FieldVar(";
			ppv v; add_break pps (0, 1);
			add_string pps (","^s^")");
			end_block pps)
		| ppv(SubscriptVar(v, e)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SubscriptVar("; add_break pps (0, 1);
			ppv v; add_string pps ","; add_break pps (0, 1);
			ppe e; add_string pps ")"; add_break pps (0, 0);
			end_block pps)
		and ppe(UnitExp _) = add_string pps "UnitExp"
		| ppe(NilExp _) = add_string pps "NilExp"
		| ppe(IntExp(n, _)) = add_string pps (Int.toString n)
		| ppe(StringExp(s, _)) = add_string pps ("\""^s^"\"")
		| ppe(BreakExp _) = add_string pps "BreakExp"
		| ppe(VarExp(v, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "VarExp("; add_break pps (0, 1);
			ppv v; add_break pps (0, 0);
			add_string pps ")"; add_break pps (0, 0);
			end_block pps)
		| ppe(OpExp({left, oper, right}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "OpExp{"; add_break pps (0, 0);
					add_string pps "left=";
					ppe left; add_string pps ","; add_break pps (0, 1);
			add_string pps "oper=";
			add_string pps
				(case oper of
				Plus => "Plus" | Minus => "Minus"
				| Times => "Times" | Div => "Div"
				| Eq => "Eq" | Neq => "Neq"
				| Lt => "Lt" | Leq => "Leq"
				| Gt => "Gt" | Geq => "Geq");
			add_string pps ","; add_break pps (0, 1);
			add_string pps "right="; ppe right; add_string pps "}";
			end_block pps)
		| ppe(WhileExp({test, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "WhileExp{test=";
			ppe test; add_string pps ","; add_break pps (0, 0);
			add_string pps "body=";
			add_string pps "body="; ppe body; add_string pps "}";
			end_block pps)
		| ppe(ForExp({var, escape, lo, hi, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "ForExp{var=";
			add_string pps var; add_string pps ","; add_break pps (0, 0);
			add_string pps "escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps ","; add_break pps (0, 0);
			add_string pps "lo="; ppe lo; add_string pps ",";
			add_break pps (0, 0);
			add_string pps "hi="; ppe hi; add_string pps ",";
			add_break pps (0, 0);
			add_string pps "body="; ppe body; add_string pps "}";
			add_break pps (0, 0);
			end_block pps)
		| ppe(AssignExp({var, exp}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "AssignExp{"; add_break pps (0, 1);
			add_string pps "var=";
			ppv var; add_string pps ","; add_break pps (0, 1);
			add_string pps "exp="; ppe exp; add_string pps "}";
			end_block pps)
		| ppe(IfExp({test, then', else', oper}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "IfExp{"; add_break pps (0, 0); 
			add_string pps "test=";
			ppe test; add_string pps ","; add_break pps (0, 0);
			add_string pps "then'=";
			ppe then'; add_string pps ","; add_break pps (0, 0);
			add_string pps "else'=";
			case else' of SOME e => ppe e | NONE => add_string pps "NONE";
			add_break pps (0, 0);
			add_string pps ", oper=";
			add_string pps (case oper of And => "AND" | Or => "OR" | If => "IF");
			add_break pps (0, 0);
			add_string pps "}";
			end_block pps)
		| ppe(CallExp({func, args}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "CallExp{"; add_break pps (0, 0);
			add_string pps ("func="^func^","); add_break pps (0, 0);
			add_string pps "args=[";
			List.app (fn e => (ppe e; add_break pps (0, 0))) args;
			add_string pps "]}";
			add_break pps (0, 0);
			end_block pps)
		| ppe(SeqExp(explist, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SeqExp([";
			List.app (fn e => (ppe e; add_break pps (0, 0))) explist;
			add_string pps "])"; add_break pps(0, 0);
			end_block pps)
		| ppe(RecordExp({fields, typ}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "RecordExp{"; add_break pps (0, 1);
			add_string pps "fields=["; add_break pps (0, 1);
			List.app
				(fn(s,e)=> (add_string pps ("("^s^",");
					ppe e; add_string pps ")"; add_break pps (0, 0)))
				fields;
			add_string pps "],"; add_break pps (0, 1);
			add_string pps "typ=";
			add_string pps typ;
			add_string pps "}";
			end_block pps)
		| ppe(ArrayExp({typ, size, init}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "ArrayExp{test=";
			add_string pps (typ^","); add_break pps (0, 1);
			add_string pps "size=";
			ppe size; add_string pps ","; add_break pps (0, 1);
			add_string pps "init=";
			ppe init; add_string pps "}";
			end_block pps)
		| ppe(LetExp({decs, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "LetExp{decs=[";
				List.app ppd decs;
			add_string pps "],"; add_break pps (0, 1);
			add_string pps "body=";
			ppe body; add_string pps "}";
			end_block pps)
	in
		begin_block pps INCONSISTENT 0; 
		ppe e0;
		end_block pps
	end
val ppstrm =
	PP.mk_ppstream {
			consumer=fn s=>TextIO.output(TextIO.stdOut, s), 
			linewidth = 79,
			flush=fn()=>TextIO.flushOut TextIO.stdOut
	}

fun exprAst e =
	(ppexpr ppstrm e;
	flush_ppstream ppstrm;
	TextIO.output(TextIO.stdOut, "\n"))
	
fun stringFromExp exp =
			case exp of
				VarExp (var, pos) => stringFromVar var
			| UnitExp pos => ""
			| NilExp pos => "nil"
			| IntExp (i, pos) => Int.toString i
			| StringExp (str, pos) => str
			| CallExp ({func, args}, pos) => 
					func ^ "(" ^ List.foldl (fn (e, s) => s ^ stringFromExp e) "" args ^ ", " ^ "\b)"
			| OpExp ({left, oper, right}, pos) =>
					"(" ^ stringFromExp left ^ " " ^ stringFromOper oper ^ " " ^ stringFromExp right ^ ")"
			| RecordExp ({fields, typ}, pos) =>
					typ ^ "{" ^ List.foldl (fn ((f,e), s) => s ^ f ^ "=" ^ stringFromExp e ^ ", ") "" fields ^ "\b}"
			| SeqExp (expl, pos) => "(" ^ List.foldl (fn (e,s) => s ^ stringFromExp e ^ "; ") "" expl ^ "\b)"
			| AssignExp ({var, exp}, pos) =>
					stringFromVar var ^ " := " ^ stringFromExp exp
			| IfExp ({test: exp, then': exp, else': exp option, oper: ifop}, pos) =>
					"if (" ^ stringFromExp test ^ ") then " ^ stringFromExp then' ^ 
					( case else' of SOME e => " else " ^ stringFromExp e | NONE => "" )
			| WhileExp ({test, body}, pos) =>
					"while (" ^ stringFromExp test ^ ") do " ^ stringFromExp body
			| ForExp ({var, escape, lo, hi, body}, pos) =>
				  "for (" ^ var ^ ":= " ^ stringFromExp lo ^ " to " ^ stringFromExp hi ^ " do " ^ stringFromExp body
			| LetExp ({decs, body}, pos) =>
					"let ...decs... in " ^ stringFromExp body ^ " end"
			| BreakExp pos => "break"
			| ArrayExp ({typ, size, init}, pos) =>
					typ ^ "[" ^ stringFromExp size ^ "] of " ^ stringFromExp init
			
		and stringFromVar var =
			case var of
				TigerAbs.SimpleVar x => x
			|	TigerAbs.FieldVar (v,x) => stringFromVar v ^ "." ^ x
			| TigerAbs.SubscriptVar (v,e) => stringFromVar v ^ "[" ^ stringFromExp e ^ "]"
		
		and stringFromOper oper = 
			case oper of
				TigerAbs.Plus => "+"
			|	TigerAbs.Minus => "-"
			|	TigerAbs.Times => "*"
			|	TigerAbs.Div => "/"
			|	TigerAbs.Eq => "="
			|	TigerAbs.Neq => "<>"
			|	TigerAbs.Lt => "<"
			|	TigerAbs.Leq => "<="
			|	TigerAbs.Gt => ">"
			|	TigerAbs.Geq => ">="
			
	fun ppIrExp e =
		case e of
			CONST n => "CONST " ^ Int.toString n
		|	NAME l => "NAME " ^ labelname l
		|	TEMP t => "TEMP " ^ tempname t
		|	BINOP (oper, e1, e2) => "BINOP (" ^ ppIrBinop oper ^ ", " ^ ppIrExp e1 ^ ", " ^ ppIrExp e2 ^ ")"
		|	MEM e => "MEM (" ^ ppIrExp e ^ ")"
		|	CALL (e, el) => "CALL (" ^ ppIrExp e ^ ", [" ^ List.foldl (fn (x, y) => y ^ ppIrExp x ^ ", ") "" el ^ "\b\b])"
		| ESEQ (s, e) => "ESEQ (" ^ ppIrStm s ^ ", " ^ ppIrExp e ^ ")"
	
	and ppIrStm s =
		case s of
			MOVE (e1, e2) => "MOVE (" ^ ppIrExp e1 ^ ", " ^ ppIrExp e2 ^ ")"
		| JUMP (e, ll) => "JUMP (" ^ ppIrExp e ^ ", [" ^ List.foldl (fn (x, y) => y ^ labelname x ^ ", ") "" ll ^ "\b\b])"
		| EXP e => "EXP (" ^ ppIrExp e ^ ")"
		| CJUMP (oper, e1, e2, t1, t2) => 
				"CJUMP (" ^ ppIrRelop oper ^ ", " ^ ppIrExp e1 ^ ", " ^ ppIrExp e2 ^ ", " ^ labelname t1 ^ ", " ^ labelname t2 ^ ")"
		| SEQ (s1, s2) => "SEQ (" ^ ppIrStm s1 ^ ", " ^ ppIrStm s2 ^ ")"
		| LABEL l => "LABEL (" ^ labelname l ^ ")"
	
	and ppIrBinop oper =
		case oper of
			PLUS => "PLUS" 
		| MINUS => "MINUS" 
		| MUL => "MUL" 
		| DIV => "DIV"
		| AND => "AND" 
		| OR => "OR" 
		| XOR => "XOR"
		| LSHIFT => "LSHIFT" 
		| RSHIFT => "RSHIFT" 
		| ARSHIFT => "ARSHIFT"
	
	and ppIrRelop oper =
		case oper of
			EQ => "EQ" 
		| NE => "NE" 
		| LT => "LT" 
		| LE => "LE" 
		| GT => "GT" 
		| GE => "GE"
		| ULT => "ULT" 
		| ULE => "ULE" 
		| UGT => "UGT" 
		| UGE => "UGE"

fun printtree (outstream, s0) =
	let 
		fun say s =  BasicIO.output(outstream,s)
	  fun sayln s= (say s; say "\n") 
	
	  fun indent 0 = ()
	    | indent i = (say " "; indent(i-1))
	
	  fun stm(SEQ(a,b),d) =
	          (indent d; sayln "SEQ("; stm(a,d+1); sayln ","; stm(b,d+1); say ")")
	    | stm(LABEL lab, d) = (indent d; say "LABEL "; say (labelname lab))
	    | stm(JUMP (e,_), d) =  (indent d; sayln "JUMP("; exp(e,d+1); say ")")
	    | stm(CJUMP(r,a,b,t,f),d) = (indent d; say "CJUMP(";
					relop r; sayln ",";
					exp(a,d+1); sayln ","; exp(b,d+1); sayln ",";
					indent(d+1); say(labelname t); 
					say ","; say (labelname f); say ")")
	    | stm(MOVE(a,b),d) = (indent d; sayln "MOVE("; exp(a,d+1); sayln ",";
				    exp(b,d+1); say ")")
	    | stm(EXP e, d) = (indent d; sayln "EXP("; exp(e,d+1); say ")")
	
	  and exp(BINOP(p,a,b),d) = (indent d; say "BINOP("; binop p; sayln ",";
				       exp(a,d+1); sayln ","; exp(b,d+1); say ")")
	    | exp(MEM(e),d) = (indent d; sayln "MEM("; exp(e,d+1); say ")")
	    | exp(TEMP t, d) = (indent d; say "TEMP "; say (tempname t))
	    | exp(ESEQ(s,e),d) = (indent d; sayln "ESEQ("; stm(s,d+1); sayln ",";
				  exp(e,d+1); say ")")
	    | exp(NAME lab, d) = (indent d; say "NAME "; say (labelname lab))
	    | exp(CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
	    | exp(CALL(e,el),d) = (indent d; sayln "CALL("; exp(e,d+1);
				   app (fn a => (sayln ","; exp(a,d+2))) el;
				   say ")")
	
	  and binop PLUS = say "PLUS"
	    | binop MINUS = say "MINUS"
	    | binop MUL = say "MUL"
	    | binop DIV = say "DIV"
	    | binop AND = say "AND"
	    | binop OR = say "OR"
	    | binop LSHIFT = say "LSHIFT"
	    | binop RSHIFT = say "RSHIFT"
	    | binop ARSHIFT = say "ARSHIFT"
	    | binop XOR = say "XOR"
	
	  and relop EQ = say "EQ"
	    | relop NE = say "NE"
	    | relop LT = say "LT"
	    | relop GT = say "GT"
	    | relop LE = say "LE"
	    | relop GE = say "GE"
	    | relop ULT = say "ULT"
	    | relop ULE = say "ULE"
	    | relop UGT = say "UGT"
	    | relop UGE = say "UGE"
	
	 in  
	 	stm(s0,0); sayln ""; BasicIO.flush_out outstream
	 end
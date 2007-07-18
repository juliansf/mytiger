open Scanner
open Parser
open TigerProgName
open TigerLineNumber
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun parseError e lexbuf= 
		case e of
		  Fail s => raise Fail s
		|	Match => Error ( ErrorParsingError (Lexing.getLexeme lexbuf), Line() )
		| Parsing.ParseError _ => Error ( ErrorParsingError (Lexing.getLexeme lexbuf), Line() )
		| e => raise e

fun main(tigername, args) =
	let	
		val arbol = ref false
		val escapes = ref false
		val ir = ref false
		val arbol_ir = ref false
		val canon = ref false
		val arbol_canon = ref false
		val code = ref false
		val code_list = ref false
		val flow = ref false
		val asm = ref false
		val output = ref false
		val inputs = ref []
		
		fun option (arg, rel) =
			case arg of
				"-arbol" => (arbol := true; false)
			| "-escapes" => (escapes := true; false)
			| "-ir" => (ir := true; false)
			| "-irtree" => (arbol_ir := true; false)
			| "-canon" => (canon := true; false)
			| "-canontree" => (arbol_canon := true; false)
			| "-canon-all" => (escapes := true; ir := true; canon := true; false)
            | "-code" => (code := true; false)
            | "-codelist" => (code_list := true; false)
            | "-code-all" => (escapes := true; ir := true; canon := true; code := true; false)
			| "-flow" => (flow := true; false)
			| "-s" => (asm := true; false)
			| "-o" => (output := true; true)
			| arg => 
					(if String.isPrefix "-" arg 
					then TigerError.ErrorUnrecognizedOption tigername arg
					else if !output then outputName := arg else inputs := arg :: (!inputs); false)
		
		fun parseInput file =
			let 
				val input = open_in file 
					handle _ => raise ErrorFileNotFound file
				
				val lexbuf = lexstream input
			in
				progName := file;
				prog Token lexbuf
					handle e => parseError e lexbuf
			end
			
		(* Parseamos los argumentos *)
		val _ = List.foldl option false args
		
		(* Parseamos las entradas y construimos el AST *)
		val expr = 
			case !inputs of
				[] => raise ErrorNoInputFiles	tigername
			| [file] => parseInput file
			| files =>  raise Fail (tigername ^ ": lo siento, actualmente no soporto multiples fuentes :(.\n")
	in	
		if !escapes then tigerescap.findEscape expr else ();
		
		if !arbol then tigerpp.exprAst expr else ();

		if !ir then 
			let 
				val lfrag = TigerSemant.transProg expr 
			in
				if !arbol_ir then 
					(print "\n::: IR TREE :::\n"; List.app tigerpp.ppfrag lfrag) else ();
				
				if !canon then
					let
						val lfrag = TigerCanon.canonize lfrag
					in
						if !arbol_canon then 
							(print "\n::: CANONICAL TREE :::\n"; List.app tigerpp.ppCanonFrag lfrag) else ();
						
						if !code then
    						let
                                fun auxCode (TigerCanon.FUNC {body, frame}) = List.map (fn x => TigerCodegen.codegen frame x) body
                                  | auxCode (TigerCanon.LITERAL _) = []     (* ARREGLARME!!! VER QUE HACER CON LOS LITERALES *)

    						    val linstr = List.map (List.concat o auxCode) lfrag
    						in
    						    if !code_list then List.app (print o (TigerAssem.format TigerTemp.tempname)) (List.concat linstr) else ()
    						end
    					else ()
					end
				else ()
			end
		else ()
	end	handle e => ShowErrors e

val _ = main( CommandLine.name(), CommandLine.arguments() )

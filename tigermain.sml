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
		| e => raise e

fun main(tigername, args) =
	let	
		val arbol = ref false
		val escapes = ref false
		val ir = ref false
		val arbol_ir = ref false
		val canon = ref false
		val code = ref false
		val flow = ref false
		val inter = ref false
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
			| "-flow" => (flow := true; false)
			| "-inter" => (inter := true; false)
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
			
		val _ = if !escapes then tigerescap.findEscape expr else ()
		val _ = if !arbol then tigerpp.exprAst expr else ()
	in	
		if !ir then 
			let 
				val irtree = TigerSemant.checkSemant expr 
				val _ = if !arbol_ir then print ((
						case irtree of TigerTranslate.NN => "NN" | t => tigerpp.ppIrExp(TigerTranslate.unEx t))^"\n") else ()
			in
				()
			end
		else ()
	end	handle e => ShowErrors e

val _ = main( CommandLine.name(), CommandLine.arguments() )

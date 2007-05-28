{
open TigerError
open TigerLineNumber
open Parser

(* Contador de comentarios *)
local val cnt = ref 0
in
  fun incCom () = cnt := !cnt+1
  fun decCom () = (cnt := !cnt-1; !cnt > 0)
end

(* Idetificadores y Palabras Claves *)
fun idKeyword s =
	let val l = Line () in
	  case s of
	  	"var" => VAR l
	  | "type" => TYPE l
	  | "function" => FUNCTION l
	  | "array" => ARRAY l
	  | "of" => OF l
	  | "let" => LET l
	  | "in" => IN l
	  | "end" => END l
	  | "if" => IF l
	  | "then" => THEN l
	  | "else" => ELSE l
	  | "for" => FOR l
	  | "to" => TO l
	  | "while" => WHILE l
	  | "do" => DO l
	  | "break" => BREAK l
	  | "nil" => NIL l
		| _ => ID { data=s, pos=l }
	end

(* Obtenemos un numero a partir de un lexbuf *)
fun atoi s = (valOf o Int.fromString) s

(* Caracteres escapados *)
fun hex n = "\\x" ^ (if n <= 15 then "0" else "") ^ (Int.fmt StringCvt.HEX n)

fun escape1 s =
    let val l = explode s
    in 
    	case l of
		  	[_,_,c] => "\\x"^hex( ord(c) - ord(#"@") )
		 	| _ => Error ( ErrorInternalError "problemas con Scanner.escape1!", Line() )
    end
	
fun escape2 s = hex (ord (valOf (Char.fromString s)))

}
let D = [`0`-`9`]
let SPACE = [` ``\t``\r``\^L`]
let L = [`A`-`Z``a`-`z`]
let LDU = [`A`-`Z``a`-`z``0`-`9``_`]

rule Token =
  parse eof               { EOF } (* Retornamos un token llamado EOF *)
  | SPACE+   							{ Token lexbuf }
  | `\n`                  { incLine (); Token lexbuf }
  
  (* Inicio de un Comentario *)
  | "/*"                  {Comment lexbuf; Token lexbuf}
  
  (* Simbolos Especiales *)
  | `+`                   { PLUS (Line()) }
  | `-`                   { MINUS (Line()) }
  | `*`                   { TIMES (Line()) }
  | `/`                   { DIV (Line()) } 
  | ":="                  { ASS (Line()) }
  | `=`                   { EQ (Line()) }
  | "<>"				  				{ NEQ (Line()) }
  | `>`                   { GT (Line()) }
  | ">="                  { GEQ (Line()) }
  | `<`                   { LT (Line()) }
  | "<="                  { LEQ (Line()) }
  |	`&`										{ AMPERSAND (Line()) }
  | `|`										{ PIPE (Line()) }
  | `(`                   { LP (Line()) }
  | `)`                   { RP (Line()) }
  | `{`                   { LB (Line()) }
  | `}`                   { RB (Line()) }
  |	`[`										{ LSB (Line()) }
  |	`]`										{ RSB (Line()) }
  | `:`                   { COLON (Line()) }
  | `;`										{ SEMICOL (Line()) }
  | `.`										{ DOT (Line()) }
  | `,`										{ COMMA (Line()) }

  (* Palabras Claves e Identificadores *)
  | [`a`-`z`]+            { idKeyword (getLexeme lexbuf) }
  | L LDU*								{ ID { data=getLexeme lexbuf, pos=Line() } }

  (* Numeros *)
  | "0"                   { NUM { data=0, pos=Line() } }
  | [`1`-`9`][`0`-`9`]*   { NUM { data=atoi (getLexeme lexbuf), pos=Line() } }

  (* Inicio de una String *)
  | `"`                  { STRING { data=String1 lexbuf, pos=Line() } }

(* Comentarios *)
and Comment =
  parse eof               { Error ( ErrorUnterminatedComment, 0 ) }
  | `\n`                  { incLine (); Comment lexbuf }
  | "/*"                  { incCom (); Comment lexbuf }
  | "*/"                  { if decCom () then Comment lexbuf else () }
  | _                     { Comment lexbuf }

(* Strings *)
and String1 =
  parse eof               { Error ( ErrorEOFUnterminatedString, 0 ) }
  | `\n`                  { Error ( ErrorNLUnterminatedString, Line() ) }
  | `"`	                  { "" }
  | "\\"[`"``\\`]					{ getLexeme lexbuf ^ String1 lexbuf }
  | "\\"[`n``r``t``b`]		{ getLexeme lexbuf ^ String1 lexbuf }
  | "\\^"[`@`-`_`]     		{ escape1 (getLexeme lexbuf) ^ String1 lexbuf }
  | "\\"D D D							{ escape2 (getLexeme lexbuf) ^ String1 lexbuf }    
  | "\\"                  { String2 lexbuf; String1 lexbuf }
  | _											{ let val s = getLexeme lexbuf 
  													in 
  														if s > "\^_" then s ^ String1 lexbuf 
  														else Error ( ErrorIllegalCharInString s, Line() ) 
  													end }
and String2 =
  parse eof               { Error ( ErrorEOFUnterminatedString, 0 ) }
  | `\n`                  { incLine(); String2 lexbuf }
  | "\\"                  { () }
  | SPACE+			          { String2 lexbuf }
  | _                     { Error ( ErrorInvalidCharBetweenBars (getLexeme lexbuf), Line() ) }
;

{

(* Contador de lineas de codigo *)
local val line = ref 0 
in
  fun incNL () = line := !line + 1
end

(* Contador de comentarios *)
local val cnt = ref 0
in
  fun incCom () = cnt := !cnt+1
  fun decCom () = cnt := !cnt-1; !cnt > 0
end

(* Idetificadores y Palabras Claves *)

(* Tabla de palabras claves *)
val tPC = Polyhash.mkPolyTable( 30, Empty )
val _ = 
    List.app ( fn (c,v) => Polyhash.insert tPC (c,v))
	     [ ("var", VAR),
	       ("type", TYPE),
	       ("array", ARRAY),
	       ("of", OF), (* Es un keyword? *)
	       ("let", LET),
	       ("in", IN),
	       ("end",END),
	       ("if", IF),
	       ("then", THEN),
	       ("else",ELSE),
	       ("for", FOR),
	       ("while", WHILE)]
	       

(* Diferenciamos entre palabras claves e identificadores *)
fun idKeyword lexbuf =
    let val s = getLexeme lexbuf
	
		  
    in
	case Polyhash.peek tPC s of
	    SOME v => v
	  | NONE => ID s
    end

(* Obtenemos un numero a partir de un lexbuf *)
fun atoi lexbuf =
    (valOf o Int.fromString) (getLexeme lexbuf)



(* Caracteres escapados *)
fun escape1 lexbuf =
    let val l = explode (getLexeme lexbuf)
    in case l of
	   [_,_,c] => "\\x"^hex( ord(c) - ord(#"@") )
	 | _ => raise Fail SFShouldNeverHappens1
    end

fun escape2 lexbuf =
    let val l = explode (getLexeme lexbuf)
    in case l of
	   _::[l] => "\\x"^()

}

rule Token =
  parse eof               { EOF } (* Retornamos un token llamado EOF *)
  | [` ``\t``\r``\b`]     { Token lexbuf } (* Ojo con el \r! *)
  | `\n`                  { incNL (); Token lexbuf }
  
  (* Simbolos Especiales *)
  | `+`                   { PLUS }
  | `-`                   { MINUS }
  | `*`                   { TIMES }
  | `/`                   { DIV } (* Sin problemas con este y los comments? *)
  | ":="                  { ASS }
  | `=`                   { EQ }
  | `>`                   { GT }
  | ">="                  { GEQ }
  | `<`                   { LT }
  | "<="                  { LEQ }
  | `(`                   { LP }
  | `)`                   { RP }
  | `{`                   { LB }
  | `}`                   { RB }
  | `:`                   { COLON }

  (* Palabras Claves e Identificadores *)
  | [`a`-`z`]+            { idKeyword lexbuf }

  (* Numeros *)
  | "0"                   { NRO 0 }
  | [`1`-`9`][`0`-`9`]*   { NRO (atoi lexbuf) }

  (* Inicio de un Comentario *)
  | "/*"                  {Comment lexbuf; Token lexbuf}

  (* Inicio de una String *)
  | "\""                  { TEXTO (String1 lexbuf) }

(* Comentarios *)
and Comment =
  parse eof               { raise Fail SFUnterminatedComment }
  | `\n`                  { incNL (); Comment lexbuf }
  | "/*"                  { incCom (); Comment lexbuf }
  | "*/"                  { if decCom () then Comment lexbuf }
  | _+                    { Comment lexbuf }

(* Strings *)
and String1 =
  parse eof               { raise Fail SFMissingTerminatingChar }
  | `\n`                  { raise Fail SFMissingTerminatingChar }
  | "\""                  { "" }
  | "\\\""                { "\""^(String1 lexbuf) }
  | "\\n"                 { "\n"^(String1 lexbuf) }
  | "\\r"                 { "\r"^(String1 lexbuf) }
  | "\\t"                 { "\t"^(String1 lexbuf) }
  | "\\b"                 { "\b"^(String1 lexbuf) }
  | "\\^"[`@``A`-`Z`]     { (escape1 lexbuf)^(String1 lexbuf)}
  | "\\"[`0`-`9`][`0`-`9`][`0`-`9`] { (escape2 lexbuf)^(String1 lexbuf) }    
  | "\\"                  { String2 lexbuf; String1 lexbuf }
and String2 =
  parse eof               { raise Fail SFMissingTerminatingChar }
  | `\n`                  { incNL (); String2 lexbuf }
  | "\\"                  { () }
  | [` ``\t``\r`]         { String2 lexbuf }
  | _                     { raise Fail SFInvalidCharBetweenBars }
;

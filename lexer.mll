{
  open Lexing ;;
  type token =
  | FUN
  | MATCH
  | WITH
  | TYPE
  | OF
  | LET
  | REC
  | UNIT
  | NOT
  | HOLE
  | REFINES
  | EQUAL
  | ARROW
  | COMMA
  | COLON
  | SEMI
  | STAR
  | PIPE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | AND
  | OR
  | LID of string
  | EOF
  | INT of int
  | TICK of string
  | UID of string
  | PROJ of int

  let reserved_map = [
    ("fun",   FUN) ;
    ("match", MATCH) ;
    ("with",  WITH) ;
    ("type",  TYPE) ;
    ("of",    OF) ;
    ("let",   LET) ;
    ("rec",   REC) ;
    ("unit",  UNIT) ;
    ("not",   NOT)
  ]

  let symbols_map = [
    ("?",   HOLE) ;
    ("|>",  REFINES) ;
    ("=",   EQUAL) ;
    ("->",  ARROW) ;
    (",",   COMMA) ;
    (":",   COLON) ;
    (";",   SEMI) ;
    ("*",   STAR) ;
    ("|",   PIPE) ;
    ("(",   LPAREN) ;
    (")",   RPAREN) ;
    ("[",   LBRACKET) ;
    ("]",   RBRACKET) ;
    ("/\\", AND) ;
    ("\\/", OR)
  ]

  (* create a token *)
  let create_token lexbuf =
    let str = lexeme lexbuf in
    match List.assoc_opt str reserved_map with
    | Some e -> e
    | None -> LID str

  (* create a symbol. *)
  let create_symbol lexbuf =
    let str = lexeme lexbuf in
    match List.assoc_opt str symbols_map with
    | Some e -> e
    | None -> failwith (Printf.sprintf "Unexpected token %s" str)

  (* create a projection. *)
  let create_proj lexbuf =
    let str = lexeme lexbuf in
    PROJ (String.sub str 1 (String.length str) |> int_of_string)

  let token_to_string = function
  | FUN -> "FUN"
  | MATCH -> "MATCH"
  | WITH -> "WITH"
  | TYPE -> "TYPE"
  | OF -> "OF"
  | LET -> "LET"
  | REC -> "REC"
  | UNIT -> "UNIT"
  | NOT -> "NOT"
  | HOLE -> "HOLE"
  | REFINES -> "REFINES"
  | EQUAL -> "EQUAL"
  | ARROW -> "ARROW"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | SEMI -> "SEMI"
  | STAR -> "STAR"
  | PIPE -> "PIPE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | AND -> "AND"
  | OR -> "OR"
  | LID s -> "LID (" ^ s ^ ")"
  | EOF -> "EOF"
  | INT i -> "INT (" ^ (string_of_int i) ^ ")"
  | TICK s -> "TICK (" ^ s ^ ")"
  | UID s -> "UID (" ^ s ^ ")"
  | PROJ i -> "PROJ (" ^ (string_of_int i) ^ ")"

  let token_list_to_string l =
    let interior = String.concat ", " (List.map token_to_string l) in
    "[" ^ interior ^ "]"
}

let newline    = '\n' | ('\r' '\n') | '\r'
let tick       = '\''
let whitespace = ['\t' ' ']
let lowercase  = ['a'-'z']
let uppercase  = ['A'-'Z']
let character  = uppercase | lowercase
let digit      = ['0'-'9']

rule token = parse
| eof         { EOF }
| digit+      { INT (int_of_string (lexeme lexbuf))}
| "#" digit+  { create_proj lexbuf }
| whitespace+ { token lexbuf }
| newline+    { token lexbuf }
| lowercase (digit | character | '_')*      { create_token lexbuf }
| tick lowercase (digit | character | '_')* { TICK (lexeme lexbuf) }
| uppercase (digit | character | '_')*      { UID (lexeme lexbuf) }
| '?' | "|>" | '=' | "->" | '*' | ',' | "/\\" | "\\/"
| ':' | ';' | '|' | '(' | ')' | '[' | ']'  { create_symbol lexbuf }
| _  { failwith "Unexpected character" }

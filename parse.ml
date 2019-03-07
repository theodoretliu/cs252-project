let get_all_tokens lexbuf =
  let rec helper lexbuf carry =
    match Lexer.token lexbuf with
    | Lexer.EOF -> carry @ [Lexer.EOF]
    | _ as e -> helper lexbuf (carry @ [e]) in
  helper lexbuf []

  let _ =
  let file = open_in "test.ml" in
  let lexbuf = Lexing.from_channel file in
  let tokens = get_all_tokens lexbuf  in
  print_endline (Lexer.token_list_to_string tokens)

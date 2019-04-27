open Types

let rec expr_to_string (e : expr) (indent : int) (newline : bool) : string =
  let prefix = String.make (indent * 2) ' ' in
  prefix ^
  begin match e with
  | EVar s -> s
  | EBase s ->
      if s then "True" else "False"
  | EIf (e1, e2, e3) ->
      "if " ^ expr_to_string e1 0 false ^ " then\n"
        ^ expr_to_string e2 (indent + 1) true
        ^ prefix ^ "else\n"
        ^ expr_to_string e3 (indent + 1) false
  | ELambda (x, e) ->
      "\\" ^ x ^ " -> \n"
        ^ expr_to_string e (indent + 1) false
  | EApp (e1, e2) ->
      expr_to_string e1 0 false ^ " "
        ^ expr_to_string e2 0 false
  | EPair (e1, e2) ->
      "(" ^ expr_to_string e1 0 false ^ ", "
        ^ expr_to_string e2 0 false ^ ")"
  | ELet (e1, e2, e3) ->
      "let " ^ expr_to_string e1 0 false ^ " =\n"
        ^ expr_to_string e2 (indent + 1) true
        ^ prefix ^ "in\n"
        ^ expr_to_string e3 indent false
| EProj _ -> failwith "not yet supported" end
^ (if newline then "\n" else "")

let expr_to_string (e : expr) = expr_to_string e 0 true

let maybe_expr_to_string (e : expr option) =
  match e with
  | None -> "program was not synthesized\n"
  | Some e -> expr_to_string e

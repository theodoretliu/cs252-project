open Tipes

let rec expr_to_string (e : expr)
                       (indent : int)
                       (newline : bool)
                       (inline : bool)
                     : string =
  let prefix = String.make (indent * 2) ' ' in
  prefix ^
  begin match e with
  | EVar s -> s
  | EBase s ->
      if s then "True" else "False"
  | EIf (e1, e2, e3) ->
      if not inline then
        "if " ^ expr_to_string e1 0 false true ^ " then\n"
          ^ expr_to_string e2 (indent + 1) true inline
          ^ prefix ^ "else\n"
          ^ expr_to_string e3 (indent + 1) false inline
      else
        "(if " ^ expr_to_string e1 0 false true ^ " then "
          ^ expr_to_string e2 0 false true ^ " else "
          ^ expr_to_string e3 0 false true ^ ")"
  | ELambda (x, e) ->
      "\\" ^ x ^ " -> \n"
        ^ expr_to_string e (indent + 1) false inline
  | EApp (e1, e2) ->
      expr_to_string e1 0 false inline ^ " "
        ^ expr_to_string e2 0 false inline
  | EPair (e1, e2) ->
      "(" ^ expr_to_string e1 0 false inline ^ ", "
        ^ expr_to_string e2 0 false inline ^ ")"
  | ELet (e1, e2, e3) ->
      if not inline then
        "let " ^ expr_to_string e1 0 false inline ^ " =\n"
          ^ expr_to_string e2 (indent + 1) true inline
          ^ prefix ^ "in\n"
          ^ expr_to_string e3 indent false inline
      else
        "(let " ^ expr_to_string e1 0 false true ^ " = "
          ^ expr_to_string e2 0 false true ^ " in "
          ^ expr_to_string e3 0 false true ^ ")"
  | EProj (i, e) ->
      "Tuple." ^ (if i = 1 then "first" else "second") ^ " "
        ^ expr_to_string e 0 false true
  | ENil -> "[]"
  | ECons (h, t) ->
      expr_to_string h 0 false inline ^ " :: " ^ expr_to_string t 0 false inline
  | EFix (f, x, e) ->
      f ^ " " ^ x ^ " =\n"
        ^ expr_to_string e (indent + 1) false inline
  | EMatch (e, cases) ->
      let case_strings = List.map (fun (e1, e2) ->
        expr_to_string e1 (indent + 1) false true ^ " ->\n"
          ^ expr_to_string e2 (indent + 2) false true) cases in
      "case " ^ expr_to_string e 0 false inline ^ " of\n"
        ^ String.concat "\n" case_strings end
  ^ (if newline then "\n" else "")

let expr_to_string (e : expr) = expr_to_string e 0 true false

let maybe_expr_to_string (e : expr option) =
  match e with
  | None -> "program was not synthesized\n"
  | Some e -> expr_to_string e

let rec refinement_to_string (r : refinement) : string =
  match r with
  | RBase b -> string_of_bool b
  | RArrow (r1, r2) ->
      "RArrow (" ^ refinement_to_string r1
        ^ ", " ^ refinement_to_string r2 ^ ")"
  | RUnion (r1, r2) ->
      "RUnion (" ^ refinement_to_string r1
        ^ ", " ^ refinement_to_string r2 ^ ")"
  | RIntersection (r1, r2) ->
      "RIntersection (" ^ refinement_to_string r1
        ^ ", " ^ refinement_to_string r2 ^ ")"
  | RProduct (r1, r2) ->
      "RProduct (" ^ refinement_to_string r1
        ^ ", " ^ refinement_to_string r2 ^ ")"
  | RBottom -> "RBottom"
  | RCons (r1, r2) ->
      "RCons (" ^ refinement_to_string r1
        ^ ", " ^ refinement_to_string r2 ^ ")"
  | RNil -> "RNil"
  | RNot _ -> failwith "not implemented"

let context_to_string (c : context) : string =
  List.map (fun (x, r) -> x ^ ":" ^ refinement_to_string r) c |> String.concat ", "

let world_to_string ((c, r) : world) : string =
  "<" ^ context_to_string c ^ " TTT " ^ refinement_to_string r ^ ">"

let worlds_to_string (ws : worlds) : string =
  List.map world_to_string ws |> String.concat ""

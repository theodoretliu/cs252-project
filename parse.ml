let get_all_tokens lexbuf =
  let rec helper lexbuf carry =
    match Lexer.token lexbuf with
    | Lexer.EOF -> carry @ [Lexer.EOF]
    | _ as e -> helper lexbuf (carry @ [e]) in
  helper lexbuf []

  (* let _ = *)
  (* let file = open_in "test.ml" in
  let lexbuf = Lexing.from_channel file in
  let tokens = get_all_tokens lexbuf  in
  print_endline (Lexer.token_list_to_string tokens) *)

type refinement =
  | Base of bool
  | Arrow of refinement * refinement
  | Union of refinement * refinement
  | Intersection of refinement * refinement
  | Product of refinement * refinement
  | Bottom

type value =
  | VVar of string
  | VBase of bool
  | VLambda of string * expr
  | VPair of value * value
and expr =
  | EVar of string
  | EBase of bool
  | EIf of expr * expr * expr
  | ELambda of string * expr
  | EApp of expr * expr
  | EPair of expr * expr
  | EProj of int * expr
  | ELet of string * expr * expr

type context = (string * refinement) list
type world = context * refinement
type worlds = world list

(* subtypes *)
let (<<) (a : refinement) (b : refinement) : bool =
  a = b

let rec (|>>) r' r =
  let prj_left (r : refinement) (r' : refinement) : bool =
    match r' with
    | Intersection (r1, r2) -> r1 |>> r'
    | _ -> false in
  let prj_right (r : refinement) (r' : refinement) : bool =
    match r' with
    | Intersection (r1, r2) -> r2 |>> r'
    | _ -> false in
  prj_left r' r || prj_right r' r || r' = r

let rec prj (c : context) (x : string) (r : refinement) : bool =
  match List.assoc_opt x c with
  | None -> false
  | Some t -> t |>> r

let s_all (w : worlds) : expr option =
  None

let s_ctx (w : worlds) : expr option =
  failwith "not implemented"

let s_rarrow (w : worlds) : expr option =
  failwith "not implemented"

let s_larrow (w : worlds) : expr option =
  failwith "not implemented"

let s_bot (w : worlds) : expr option =
  let rec contains_bottom (r : refinement) : bool =
    match r with
    | Bottom -> true
    | Intersection (r1, r2) -> contains_bottom r1 || contains_bottom r2
    | _ -> false in
  let able_to_drop = List.filter (fun (_c, r) -> contains_bottom r) w in
  let filtered_worlds = List.map (fun world -> List.filter ((<>) world) w) able_to_drop in
  let results = List.map s_all filtered_worlds in
  match results with
  | [] -> None
  | h :: _t -> h

let s_ror_gen (is_or : bool) (i : int) (w : worlds) : expr option =
  assert (i = 1 || i = 2 || not is_or) ;
  let available_worlds = List.filter (fun (_, r) ->
    match r with
    | Union _ -> is_or
    | Intersection _ -> not is_or
    | _ -> false)
    w in
  let extract_and (w : world) =
    match w with
    | c, Intersection (r1, r2) -> [(c, r1); (c, r2)]
    | _ -> failwith "extract_and, impossible" in
  let extract_or i (w : world) =
    match w with
    | c, Union (r1, r2) -> [(c, if i = 1 then r1 else r2)]
    | _ -> failwith "extract_or, impossible" in
  let new_worlds = List.map (fun world ->
    let removed = List.filter (fun x -> x <> world) w in
    let addition = if is_or then extract_or i world else extract_and world in
    removed @ addition) available_worlds in
  let attempts = List.map s_all new_worlds in
  let final = List.filter (fun res -> res <> None) attempts in
  match final with
  | [] -> None
  | h :: _t -> h

let s_rand (w : worlds) : expr option = s_ror_gen false 0 w

let s_ror1 (w : worlds) : expr option = s_ror_gen true 1 w

let s_ror2 (w : worlds) : expr option = s_ror_gen true 2 w

let s_lor (w : worlds) : expr option =
  failwith "not implemented"

let s_bool (b : bool) (w : worlds) : expr option =
  if List.for_all (fun (_, r) -> Base b << r) w then Some (EBase b)
  else None

let s_true (w : worlds) : expr option = s_bool true w

let s_false (w : worlds) : expr option = s_bool false w

let s_ite (w : worlds) : expr option =
  failwith "not implemented"

let s_sample (w : worlds) : expr option =
  failwith "not implemented"

let s_rpair (w : worlds) : expr option =
  let can_use = List.for_all (fun (_c, r) ->
    match r with
    | Product _ -> true
    | _ -> false) w in
  if not can_use then None else
  let first_world = List.map (fun (c, r) ->
    match r with
    | Product (r1, _r2) -> c, r1
    | _ -> failwith "s_rpair; impossible") w in
  let second_world = List.map (fun (c, r) ->
    match r with
    | Product (_r1, r2) -> c, r2
    | _ -> failwith "s_rpair; impossible") w in
  match s_all first_world, s_all second_world with
  | Some s1, Some s2 -> Some (EPair (s1, s2))
  | _ -> None

let s_lpair (w : worlds) : expr option =
  failwith "not implemented"

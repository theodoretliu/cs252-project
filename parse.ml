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
  | RBase of bool
  | RArrow of refinement * refinement
  | RUnion of refinement * refinement
  | RIntersection of refinement * refinement
  | RProduct of refinement * refinement
  | RBottom
  | RNot of neg_refinement
and neg_refinement =
  | NRBase of bool
  | NRProduct of neg_refinement * neg_refinement
  | NRArrow of singleton * neg_refinement
  | NRBottom
  | NRIntersection of neg_refinement * neg_refinement
  | NRUnion of neg_refinement * neg_refinement
  | NRNot of neg_refinement
and singleton =
  | SBase of bool
  | SProduct of singleton * singleton

type tipe =
  | TBool
  | TProduct of tipe * tipe
  | TArrow of tipe * tipe

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
  | ELet of expr * expr * expr

module VarSet = Set.Make (struct
  type t = string
  let compare = compare
end)

type context = (string * refinement) list
type world = context * refinement
type worlds = world list

let x = ref 0

let gen_var () =
  let var = "gendvar" ^ (string_of_int !x) in
  let _ = x := !x + 1 in
  var

(* subtypes *)
let (<<) (a : refinement) (b : refinement) : bool =
  a = b

let rec (|>>) r' r =
  let prj_left (r : refinement) (r' : refinement) : bool =
    match r' with
    | RIntersection (r1, r2) -> r1 |>> r'
    | _ -> false in
  let prj_right (r : refinement) (r' : refinement) : bool =
    match r' with
    | RIntersection (r1, r2) -> r2 |>> r'
    | _ -> false in
  prj_left r' r || prj_right r' r || r' = r

let rec prj (c : context) (x : string) (r : refinement) : bool =
  match List.assoc_opt x c with
  | None -> false
  | Some t -> t |>> r

let find_common_vars (w : worlds) : string list =
  let candidate_vars_set = List.fold_left (fun acc (c, _r) ->
    let vars, _tipes = List.split c in
    let var_set = VarSet.of_list vars in
    VarSet.union acc var_set) VarSet.empty w in
  VarSet.elements candidate_vars_set

let s_all (w : worlds) : expr option =
  None

let s_ctx (w : worlds) : expr option =
  let candidate_vars = find_common_vars w in
  let can_use_var = List.map (fun x ->
    List.for_all (fun (c, r) ->
      let r' = List.assoc x c in
      (r' << r) && (r << RIntersection (RBase true, RBase false))) w)
    candidate_vars in
  let zipped = List.combine candidate_vars can_use_var in
  let filter_zipped = List.filter snd zipped in
  match filter_zipped with
  | [] -> None
  | (x, b) :: _t ->
      let _ = assert b in
      Some (EVar x)

let s_rarrow (w : worlds) : expr option =
  let can_use = List.for_all (fun (_c, r) ->
    match r with
    | RArrow _ -> true
    | _ -> false) w in
  if not can_use then None else
  let x = gen_var () in
  let new_worlds = List.map (fun (c, r) ->
    match r with
    | RArrow (r1, r2) -> (x, r1) :: c, r2
    | _ -> failwith "s_rarrow; impossible") w in
  match s_all new_worlds with
  | None -> None
  | Some s -> Some (ELambda (x, s))

let s_larrow (w : worlds) : expr option =
  let x1_cands = find_common_vars w in
  let rec extract_arrows (r : refinement) : refinement option =
    match r with
    | RIntersection (r1, r2) ->
        begin match extract_arrows r1, extract_arrows r2 with
        | Some a, _ | _, Some a -> Some a
        | _ -> None end
    | RArrow _ -> Some r
    | _ -> None in
  let arrows_cands = List.filter (fun x1 ->
    List.for_all (fun (_c, r) ->
      extract_arrows r <> None) w) x1_cands in
  let arrows = List.map (fun x1 ->
    let arrows_list = List.map (fun (_c, r) ->
      let res = extract_arrows r in
      let _ = assert (res <> None) in
      res) w in
    x1, arrows_list) arrows_cands in
  let remove_maybes_and_arrow = List.map (fun (x1, arrows_list) ->
    x1, List.map (fun maybe_arrow ->
      match maybe_arrow with
      | Some (RArrow (r1, r2)) -> (r1, r2)
      | _ -> failwith "s_larrow; impossible") arrows_list) arrows in
  let left_parts, right_parts = List.map (fun (x1, arrows_list) ->
    (x1, List.map fst arrows_list), (x1, List.map snd arrows_list)) remove_maybes_and_arrow
    |> List.split in
  let s1s = List.map (fun (x1, lefts) ->
    let new_worlds = List.combine (List.map fst w) lefts in
    x1, s_all new_worlds) left_parts in
  let x2 = gen_var () in
  let s2s = List.map (fun (x1, rights) ->
    let new_worlds = List.map2 (fun (c, r) right ->
      (x2, right) :: c, r) w rights in
    s_all new_worlds) right_parts in
  let rec get_match s1s s2s =
    match s1s, s2s with
    | [], [] -> None
    | (x1, Some s1) :: t1, Some s2 :: t2 -> Some (ELet (EVar x2, EApp (EVar x1, s1), s2))
    | _ :: t1, _ :: t2 -> get_match t1 t2
    | _ -> failwith "s_larrow; wrong lengths" in
  let _ = assert (List.length s1s = List.length s2s) in
  get_match s1s s2s

let s_bot (w : worlds) : expr option =
  let rec contains_bottom (r : refinement) : bool =
    match r with
    | RBottom -> true
    | RIntersection (r1, r2) -> contains_bottom r1 || contains_bottom r2
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
    | RUnion _ -> is_or
    | RIntersection _ -> not is_or
    | _ -> false)
    w in
  let extract_and (w : world) =
    match w with
    | c, RIntersection (r1, r2) -> [(c, r1); (c, r2)]
    | _ -> failwith "extract_and, impossible" in
  let extract_or i (w : world) =
    match w with
    | c, RUnion (r1, r2) -> [(c, if i = 1 then r1 else r2)]
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
  if List.for_all (fun (_, r) -> RBase b << r) w then Some (EBase b)
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
    | RProduct _ -> true
    | _ -> false) w in
  if not can_use then None else
  let first_world = List.map (fun (c, r) ->
    match r with
    | RProduct (r1, _r2) -> c, r1
    | _ -> failwith "s_rpair; impossible") w in
  let second_world = List.map (fun (c, r) ->
    match r with
    | RProduct (_r1, r2) -> c, r2
    | _ -> failwith "s_rpair; impossible") w in
  match s_all first_world, s_all second_world with
  | Some s1, Some s2 -> Some (EPair (s1, s2))
  | _ -> None

let s_lpair (w : worlds) : expr option =
  let x_cands = find_common_vars w in
  let rec get_product_tipes (r : refinement) : refinement option =
    match r with
    | RIntersection (r1, r2) ->
        let try_r1 = get_product_tipes r1 in
        if try_r1 <> None then try_r1 else
        get_product_tipes r2
    | RProduct (r1, r2) -> Some r
    | _ -> None in
  let x_union_cands = List.filter (fun x ->
    List.for_all (fun (c, _r) ->
      let rf = List.assoc x c in
      get_product_tipes rf <> None) w) x_cands in
  let unions = List.map (fun x ->
    x, List.map (fun (c, _r) ->
      let rf = List.assoc x c in
      let res = get_product_tipes rf in
      let _ = assert (res <> None) in
      match res with
      | Some (RProduct (rai, rbi)) -> rai, rbi
      | _ -> failwith "s_lpair; impossible") w) x_union_cands in
  let x1, x2 = gen_var (), gen_var () in
  let new_worlds = List.map (fun (x, unions) ->
    x, List.map2 (fun (rai, rbi) (c, r) ->
      (x1, rai) :: (x2, rbi) :: c, r) unions w) unions in
  let try_s = List.map (fun (x, w) ->
    x, s_all w) new_worlds in
  match try_s with
  | [] -> None
  | (x, Some s) :: _t -> Some (ELet (EPair (EVar x1, EVar x2), EVar x, s))
  | _ -> None

let rec singleton_to_neg_ref (t : singleton) : neg_refinement =
  match t with
  | SBase b -> NRBase b
  | SProduct (s1, s2) -> NRProduct (singleton_to_neg_ref s1, singleton_to_neg_ref s2)

let rec neg_ref_to_singleton (t : neg_refinement) : singleton =
  match t with
  | NRBase b -> SBase b
  | NRProduct (t1, t2) -> SProduct (neg_ref_to_singleton t1, neg_ref_to_singleton t2)
  | _ -> failwith "invalid singleton conversion"

let rec tipe_to_singleton (t : tipe) : singleton =
  match t with
  | TBool -> SBase true
  | TProduct (t1, t2) -> SProduct (tipe_to_singleton t1, tipe_to_singleton t2)
  | _ -> failwith "invaild tipe_to_singleton conversion"

let rec tipe_to_neg_ref (t : tipe) : neg_refinement =
  match t with
  | TBool -> NRBase true
  | TProduct (t1, t2) -> NRProduct (tipe_to_neg_ref t1, tipe_to_neg_ref t2)
  | TArrow (t1, t2) -> NRArrow (tipe_to_singleton t1, tipe_to_neg_ref t2)

let rec neg_norm (u : neg_refinement) (t : tipe) : neg_refinement =
  match u, t with
  | NRBase v, TBool -> NRBase v
  | NRBottom, _ -> NRBottom
  | NRIntersection (u1, u2), _ -> NRIntersection (neg_norm u1 t, neg_norm u2 t)
  | NRUnion (u1, u2), _ -> NRUnion (neg_norm u1 t, neg_norm u2 t)
  | NRProduct (u1, u2), TProduct (t1, t2) -> NRProduct (neg_norm u1 t1, neg_norm u2 t2)
  | NRArrow (u1, u2), TArrow (t1, t2) ->
      NRArrow (neg_norm (singleton_to_neg_ref u1) t1 |> neg_ref_to_singleton, neg_norm u2 t2)
  | NRNot (NRBase v), TBool -> NRBase (not v)
  | NRNot (NRBottom), _ -> NRBase true
  | NRNot (NRIntersection (u1, u2)), _ -> NRUnion (neg_norm (NRNot u1) t, neg_norm (NRNot u2) t)
  | NRNot (NRUnion (u1, u2)), _ -> NRIntersection (neg_norm (NRNot u1) t, neg_norm (NRNot u2) t)
  | NRNot (NRNot u), _ -> neg_norm u t
  | NRNot (NRProduct (u1, u2)), TProduct (t1, t2) ->
      NRUnion (
        NRProduct (neg_norm (NRNot u1) t1, tipe_to_neg_ref t2),
        NRProduct (tipe_to_neg_ref t1, neg_norm (NRNot u2) t2)
      )
  | NRNot (NRArrow (u1, u2)), TArrow (t1, t2) ->
      NRArrow (u1, neg_norm (NRNot u2) t2)
  | _ -> failwith "invlid negation normalization"

open Tipes
open Stringify

let count = ref 0

module VarSet = Set.Make (struct
  type t = string
  let compare = compare
end)

let x = ref 0

let gen_var () =
  let var = "gendvar" ^ (string_of_int !x) in
  let _ = x := !x + 1 in
  var

(* subtypes *)
let rec (<<) (a : refinement) (b : refinement) : bool =
  match b with
  | RUnion (b1, b2) -> a << b1 || a << b2
  | _ -> a = b

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
  match w with
  | [] -> []
  | (c, _r) :: _t -> List.map (fun (x, _t) -> x) c

let rec refinement_list_to_union (refs : refinement list) : refinement =
  match refs with
  | [] -> failwith "invalid list"
  | [h] -> h
  | h :: t -> RUnion (h, refinement_list_to_union t)

let rec refinement_list_to_intersection (refs : refinement list) : refinement =
  match refs with
  | [] -> RBottom
  | [h] -> h
  | h :: t -> RIntersection (h, refinement_list_to_intersection t)

let rec enumerate_possible (ll : 'a list list) =
  match ll with
  | [] -> [[]]
  | h :: t ->
      let end_poss = enumerate_possible t in
      List.map (fun x -> List.map (fun c -> x :: c) end_poss) h |> List.flatten

let rec s_all (w : worlds) (depth : int) (matches : int) (can_recurse : bool) : expr option =
  if depth = 0 then None else

  let s_ctx (w : worlds) : expr option =
    let candidate_vars = find_common_vars w in
    let try_x (x : string) : expr option =
      let is_good (c, r) =
        let r' = List.assoc x c in
        (r' << r) in
      if List.for_all is_good w then Some (EVar x) else None in
    let rec s_ctx' (xs : string list) : expr option =
      match xs with
      | [] -> None
      | x :: t ->
          match try_x x with
          | None -> s_ctx' t
          | Some s -> Some s in
    s_ctx' candidate_vars in

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
    match s_all new_worlds (depth - 1) matches can_recurse with
    | None -> None
    | Some s -> Some (ELambda (x, s)) in

  let s_larrow (w : worlds) : expr option =
    let x1_cands = find_common_vars w in
    let rec extract_arrows (r : refinement) : (refinement * refinement) list option =
      match r with
      | RIntersection (r1, r2) ->
          begin match extract_arrows r1, extract_arrows r2 with
          | None, None -> None
          | None, Some l | Some l, None -> Some l
          | Some l1, Some l2 -> Some (l1 @ l2) end
      | RArrow (rai, rbi) -> Some [(rai, rbi)]
      | _ -> None in
    let try_x1 x1 =
      let maybe_arrows = List.map (fun (c, _r) ->
        let xs_ref = List.assoc x1 c in
        extract_arrows xs_ref) w in
      if List.exists ((=) None) maybe_arrows then None else
      let try_arrows (arrows : (refinement * refinement) list) : expr option =
        let left_parts, right_parts = List.split arrows in
        let new_worlds = List.map2 (fun (c, _r) new_r -> (c, new_r)) w left_parts in
        match s_all new_worlds (depth - 1) matches can_recurse with
        | None -> None
        | Some s1 ->
            let x2 = gen_var () in
            let new_worlds = List.map2 (fun (c, r) new_r ->
              (x2, new_r) :: c, r) w right_parts in
            match s_all new_worlds (depth - 1) matches can_recurse with
            | None -> None
            | Some s2 -> Some (ELet (EVar x2, EApp (EVar x1, s1), s2)) in
      let strip_maybes = List.map (fun x ->
        match x with
        | Some s -> s
        | None -> failwith "impossible; s_larrow") maybe_arrows in
      let black_out = List.map2 (fun arrows (c, r) ->
        List.filter (fun (_r1, r2) -> r2 = r) arrows) strip_maybes w in
      let arrow_combs = enumerate_possible black_out in
      let rec try_x1' remaining_combs =
        match remaining_combs with
        | [] -> None
        | h :: t ->
            match try_arrows h with
            | None -> try_x1' t
            | Some s -> Some s in
      try_x1' arrow_combs in
    let rec s_larrow' xs =
      match xs with
      | [] -> None
      | x :: t ->
          match try_x1 x with
          | None -> s_larrow' t
          | Some s -> Some s in
    s_larrow' x1_cands in

  let s_bot (w : worlds) : expr option =
    let try_world (c, r) =
      let try_x x =
        let r = List.assoc x c in
        let rec contains_bottom (r : refinement) : bool =
          match r with
          | RBottom -> true
          | RIntersection (r1, r2) -> contains_bottom r1 || contains_bottom r2
          | _ -> false in
        contains_bottom r in
      if not (List.exists (fun (x, _r) -> try_x x) c) then None else
      let new_worlds = List.filter ((<>) (c, r)) w in
      s_all new_worlds (depth - 1) matches can_recurse in
    let rec s_bot' ws =
      match ws with
      | [] -> None
      | h :: t ->
          match try_world h with
          | None -> s_bot' t
          | Some s -> Some s in
    s_bot' w in

  let s_ror_gen (is_or : bool) (i : int) (w : worlds) : expr option =
    assert (i = 1 || i = 2 || not is_or) ;
    let try_world ((c, r) as wor : world) : expr option =
      let filtered_worlds = List.filter ((<>) wor) w in
      match r with
      | RUnion (r1, r2) ->
          if not is_or then None else
          if i = 1 then s_all ((c, r1) :: filtered_worlds) (depth - 1) matches can_recurse else
          if i = 2 then s_all ((c, r2) :: filtered_worlds) (depth - 1) matches can_recurse else
          None
      | RIntersection (r1, r2) ->
          if is_or then None else
          s_all ((c, r1) :: (c, r2) :: filtered_worlds) (depth - 1) matches can_recurse
      | _ -> None in
    let rec s_ror_gen' ws =
      match ws with
      | [] -> None
      | h :: t ->
          match try_world h with
          | None -> s_ror_gen' t
          | Some s -> Some s in
    s_ror_gen' w in

  let s_rand (w : worlds) : expr option = s_ror_gen false 0 w in

  let s_ror1 (w : worlds) : expr option = s_ror_gen true 1 w in

  let s_ror2 (w : worlds) : expr option = s_ror_gen true 2 w in

  let s_lor (w : worlds) : expr option =
    let rec try_x (r : refinement) : (refinement * refinement) option =
      match r with
      | RIntersection (r1, r2) ->
          let compute = try_x r1 in
          if compute <> None then compute else
          try_x r2
      | RUnion (r1, r2) -> Some (r1, r2)
      | _ -> None in
    let try_world ((c, r) : world) : expr option =
      let rec try_world' (c' : context) : expr option =
        match c' with
        | [] -> None
        | (x, r0) :: t ->
            match try_x r0 with
            | None -> try_world' t
            | Some (r1, r2) ->
                let remove_worlds = List.filter ((<>) (c', r)) w in
                let remove_contexts = List.filter (fun (old_x, _r) -> old_x <> x) c' in
                let new_worlds = ((x, r1) :: remove_contexts, r)
                  :: ((x, r2) :: remove_contexts, r) :: remove_worlds in
                match s_all new_worlds (depth - 1) matches can_recurse with
                | None -> try_world' t
                | Some s -> Some s in
      try_world' c in
    let rec s_lor' w =
      match w with
      | [] -> None
      | wor :: t ->
          match try_world wor with
          | None -> s_lor' t
          | Some s -> Some s in
    s_lor' w in

  let s_bool (b : bool) (w : worlds) : expr option =
    if List.for_all (fun (_, r) -> RBase b << r) w then Some (EBase b)
    else None in

  let s_true (w : worlds) : expr option = s_bool true w in

  let s_false (w : worlds) : expr option = s_bool false w in

  let s_ite (w : worlds) : expr option =
    let x_cands = find_common_vars w in
    let try_x x =
      let rec extract_true_false r =
        match r with
        | RBase x -> Some x
        | RIntersection (r1, r2) ->
            let res = extract_true_false r1 in
            if res <> None then res else
            extract_true_false r2
        | _ -> None in
      let true_falses = List.map (fun (c, r) ->
        let r = List.assoc x c in
        extract_true_false r) w in
      if List.exists ((=) None) true_falses then None else
      let worlds_and_bools = List.combine w true_falses in
      let trues = List.filter (fun (w, t) -> t = Some true) worlds_and_bools
        |> List.map fst in
      let falses = List.filter (fun (w, t) -> t = Some false) worlds_and_bools
        |> List.map fst in
      match s_all trues (depth - 1) matches can_recurse with
      | None -> None
      | Some s1 ->
          match s_all falses (depth - 1) matches can_recurse with
          | None -> None
          | Some s2 -> Some (EIf (EVar x, s1, s2)) in
    let rec s_ite' xs =
      match xs with
      | [] -> None
      | x :: t ->
          match try_x x with
          | None -> s_ite' t
          | Some s -> Some s in
    s_ite' x_cands in

  let _s_sample (w : worlds) : expr option =
    failwith "not implemented" in

  let s_rpair (w : worlds) : expr option =
    let can_use = List.for_all (fun (_c, r) ->
      match r with
      | RProduct _ -> true
      | _ -> false) w in
    if not can_use then None else
    let left_part, right_part = List.map (fun (c, r) ->
      match r with
      | RProduct (r1, r2) -> (r1, r2)
      | _ -> failwith "s_rpair; impossible") w |> List.split in
    let first_world = List.map2 (fun (c, _r) r' -> (c, r')) w left_part in
    let second_world = List.map2 (fun (c, _r) r' -> (c, r')) w right_part in
    match s_all first_world (depth - 1) matches can_recurse, s_all second_world (depth - 1) matches can_recurse with
    | Some s1, Some s2 -> Some (EPair (s1, s2))
    | _ -> None in

  let s_lpair (w : worlds) : expr option =
    let x_cands = find_common_vars w in
    let try_x x =
      let rec get_product_tipes (r : refinement) : (refinement * refinement) option =
        match r with
        | RIntersection (r1, r2) ->
            let try_r1 = get_product_tipes r1 in
            if try_r1 <> None then try_r1 else
            get_product_tipes r2
        | RProduct (r1, r2) -> Some (r1, r2)
        | _ -> None in
      let product_tipes = List.map (fun (c, _r) ->
        let r = List.assoc x c in
        get_product_tipes r) w in
      if List.exists ((=) None) product_tipes then None else
      let remove_option = List.map (fun s ->
        match s with
        | None -> failwith "s_lpair; impossible"
        | Some s -> s) product_tipes in
      let x1, x2 = gen_var (), gen_var () in
      let new_worlds = List.map2 (fun (c, r) (rai, rbi) ->
        (x1, rai) :: (x2, rbi) :: c, r) w remove_option in
      match s_all new_worlds (depth - 1) matches can_recurse with
      | None -> None
      | Some s -> Some (ELet (EPair (EVar x1, EVar x2), EVar x, s)) in
    let rec s_lpair' xs =
      match xs with
      | [] -> None
      | x :: t ->
          match try_x x with
          | None -> s_lpair' t
          | Some s -> Some s in
    s_lpair' x_cands in

  let s_fix (w : worlds) : expr option =
    let can_use = List.for_all (fun (_c, r) ->
      match r with
      | RArrow _ -> true
      | _ -> false) w in
    if not can_use then None else
    let x, f = gen_var (), gen_var () in
    let fun_ref = List.map snd w in
    let arrow_pairs = List.map (fun (_c, r) ->
      match r with
      | RArrow (ra, rb) -> ra, rb
      | _ -> failwith "impossible s_fix") w in
    let new_worlds =
      List.map2 (fun (ra, rb) (c, r) ->
        (x, ra)
          :: (f, List.filter ((<>) r) fun_ref |> refinement_list_to_intersection)
          :: c, rb) arrow_pairs w in
    match s_all new_worlds (depth - 1) matches false with
    | None -> None
    | Some s -> Some (EFix (f, x, s)) in

  let s_match_list (w : worlds) : expr option =
    if matches = 0 then None else
    let x_cands = find_common_vars w in
    let try_x x =
      let rec get_list_construct r =
        match r with
        | RCons _ | RNil -> Some r
        | RIntersection (r1, r2) ->
            let res = get_list_construct r1 in
            if res <> None then res else
            get_list_construct r2
        | _ -> None in
      let list_constructs = List.map (fun (c, _r) ->
        let r = List.assoc x c in
        get_list_construct r) w in
      if List.exists ((=) None) list_constructs then None else
      let nil_worlds = List.filter (fun (wor, con) -> con = Some RNil)
                                   (List.combine w list_constructs) |> List.map fst in
      let con_worlds = List.filter (fun (wor, con) ->
        match con with
        | Some (RCons _) -> true
        | _ -> false) (List.combine w list_constructs) in
      match s_all nil_worlds (depth - 1) (matches - 1) can_recurse with
      | None -> None
      | Some s1 ->
          let h, t = gen_var (), gen_var () in
          let new_worlds = List.map (fun ((c, r), con) ->
            match con with
            | Some (RCons (h', t')) -> (h, h') :: (t, t') :: c, r
            | _ -> failwith "impossible; s_match_list") con_worlds in
          match s_all new_worlds (depth - 1) (matches - 1) can_recurse with
          | None -> None
          | Some s2 ->
              Some (EMatch (EVar x, [
                ENil, s1 ;
                ECons (EVar h, EVar t), s2
              ])) in
    let rec s_match_list' xs =
      match xs with
      | [] -> None
      | h :: t ->
          match try_x h with
          | None -> s_match_list' t
          | Some s -> Some s in
    s_match_list' x_cands in

  let s_nil (w : worlds) : expr option =
    let can_use =
      List.for_all (fun (_c, r) -> RNil << r) w in
    if not can_use then None else Some ENil in

  let s_cons (w : worlds) : expr option =
    let can_use =
      List.for_all (fun (_c, r) ->
        match r with
        | RCons _ -> true
        | _ -> false) w in
    if not can_use then None else
    let first_worlds, second_worlds = List.map (fun (c, r) ->
      match r with
      | RCons (h, t) -> (c, h), (c, t)
      | _ -> failwith "impossible; s_cons") w |> List.split in
    match s_all first_worlds (depth - 1) matches can_recurse with
    | None -> None
    | Some s1 ->
        match s_all second_worlds (depth - 1) matches can_recurse with
        | None -> None
        | Some s2 -> Some (ECons (s1, s2)) in

  let all_rules = [
    "true", s_true ;
    "false", s_false ;
    "ctx", s_ctx ;
    "rarrow", s_rarrow ;
    "fix", s_fix ;
    "ite", s_ite ;
    "bot", s_bot ;
    "rand", s_rand ;
    "ror1", s_ror1 ;
    "ror2", s_ror2 ;
    "lor", s_lor ;
    "rpair", s_rpair ;
    "lpair", s_lpair ;
    "larrow", s_larrow ;
    "match_list", s_match_list ;
    "nil", s_nil ;
    "cons", s_cons ;
  ] in
  let rec s_all' rules =
    match rules with
    | [] -> None
    | (name, h) :: t ->
        match h w with
        | None -> s_all' t
        | Some s -> Some s in
  s_all' all_rules

let synth w limit =
  let rec synth' cur =
    if cur > limit then None else
    match s_all w cur 1 true with
    | None -> synth' (cur + 1)
    | Some s -> Some s in
  synth' 1

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

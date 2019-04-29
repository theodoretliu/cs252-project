open Rules
open Tipes
open Stringify

let bool_id = [
  [], RArrow (RBase true, RBase true) ;
  [], RArrow (RBase false, RBase false)
]

let bool_neg = [
  [], RArrow (RBase false, RBase true) ;
  [], RArrow (RBase true, RBase false)
]

let bool_or = [
  [], RArrow (RBase false, RArrow (RBase true, RBase true)) ;
  [], RArrow (RBase false, RArrow (RBase false, RBase false)) ;
  [], RArrow (RBase true, RArrow (RBase false, RBase true))
]

let bool_uncurry_and = [
  [], RArrow (RProduct (RBase false, RBase true), RBase false) ;
  [], RArrow (RProduct (RBase true, RBase true), RBase true) ;
  [], RArrow (RProduct (RBase true, RBase false), RBase false)
]

let list_head = [
  [], RArrow (RNil, RBase false) ;
  [], RArrow (RCons (RBase false, RNil), RBase false) ;
  [], RArrow (RCons (RBase true, RNil), RBase true) ;
  [], RArrow (RCons (RBase false, RCons (RBase true, RNil)), RBase false) ;
]

let not_list_head = [
  [], RArrow (RNil, RBase true) ;
  [], RArrow (RCons (RBase false, RNil), RBase true) ;
  [], RArrow (RCons (RBase true, RNil), RBase false) ;
  [], RArrow (RCons (RBase false, RCons (RBase true, RNil)), RBase true) ;
]

let list_tail = [
  [], RArrow (RNil, RNil) ;
  [], RArrow (RCons (RBase false, RNil), RNil) ;
  [], RArrow (RCons (RBase false, RCons (RBase false, RNil)),
              RCons (RBase false, RNil)) ;
]

let list_id = [
  [], RArrow (RNil, RNil) ;
  [], RArrow (RCons (RBase false, RNil), RCons (RBase false, RNil)) ;
  [], RArrow (RCons (RBase true, RNil), RCons (RBase true, RNil)) ;
  [], RArrow (RCons (RBase false, RCons (RBase false, RNil)),
              RCons (RBase false, RCons (RBase false, RNil)))
]

let not_of_list = [
  [], RArrow (RNil, RNil) ;
  [], RArrow (RCons (RBase false, RNil), RCons (RBase true, RNil)) ;
  [], RArrow (RCons (RBase true, RNil), RCons (RBase false, RNil)) ;
  [], RArrow (RCons (RBase false, RCons (RBase false, RNil)),
              RCons (RBase true, RCons (RBase true, RNil)))
]

let list_and = [
  [], RArrow (RNil, RBase true) ;
  [], RArrow (RCons (RBase true, RNil), RBase true) ;
  [], RArrow (RCons (RBase false, RNil), RBase false) ;
  [], RArrow (RCons (RBase true, RCons (RBase false, RNil)), RBase false) ;
  [], RArrow (RCons (RBase true, RCons (RBase true, RNil)), RBase true) ;
  (* [], RArrow (RCons (RBase false, RCons (RBase true, RNil)), RBase false) ; *)
  (* [], RArrow (RCons (RBase false, RCons (RBase false, RNil)), RBase false) ; *)
]

let test (name, worlds) =
  let _ = print_endline name in
  let start = Sys.time () in
  let res = synth worlds 15 in
  let end_time = Sys.time () in
  let _ = print_float (end_time -. start) in
  let _ = print_newline () in
  let _ = maybe_expr_to_string res |> print_string in
  print_newline ()

let _ =
  List.iter test [
    "bool id", bool_id ;
    "bool neg", bool_neg ;
    "bool or", bool_or ;
    "bool uncurry and", bool_uncurry_and ;
    "list head", list_head ;
    "not list head", not_list_head ;
    "list tail", list_tail ;
    "list id", list_id ;
    "not of list", not_of_list ;
    (* "list and", list_and ; *)
  ]

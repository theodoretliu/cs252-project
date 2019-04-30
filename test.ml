open Rules
open Tipes
open Stringify

let (@>) a b = RArrow (a, b)

let rec (!/) l =
  match l with
  | [] -> RNil
  | h :: t -> RCons (RBase h, !/ t)

let bool_id = [
  [], RBase true @> RBase true ;
  [], RBase false @> RBase false ;
]

let bool_neg = [
  [], RBase false @> RBase true ;
  [], RBase true @> RBase false ;
]

let bool_or = [
  [], RBase false @> RBase true @> RBase true ;
  [], RBase false @> RBase false @> RBase false ;
  [], RBase true @> RBase false @> RBase true ;
]

let bool_uncurry_and = [
  [], RProduct (RBase false, RBase true) @> RBase false ;
  [], RProduct (RBase true, RBase true) @> RBase true ;
  [], RProduct (RBase true, RBase false) @> RBase false ;
]

let bool_impl = [
  [], RBase false @> RBase false @> RBase true ;
  [], RBase false @> RBase true @> RBase true ;
  [], RBase true @> RBase false @> RBase false ;
  [], RBase true @> RBase true @> RBase true ;
]

let bool_xor = [
  [], RBase false @> RBase false @> RBase false ;
  [], RBase false @> RBase true @> RBase true ;
  [], RBase true @> RBase false @> RBase true ;
  [], RBase true @> RBase true @> RBase false ;
]

let list_head = [
  [], !/ [] @> RBase false ;
  [], !/ [false] @> RBase false ;
  [], !/ [true] @> RBase true ;
  [], !/ [false; true] @> RBase false ;
]

let not_list_head = [
  [], !/ [] @> RBase true ;
  [], !/ [false] @> RBase true ;
  [], !/ [true] @> RBase false ;
  [], !/ [false; true] @> RBase true ;
]

let list_tail = [
  [], !/ [] @> !/ [] ;
  [], !/ [false] @> !/ [] ;
  [], !/ [false; false] @> !/ [false] ;
]

let list_id = [
  [], !/ [] @> !/ [] ;
  [], !/ [false] @> !/ [false] ;
  [], !/ [true] @> !/ [true] ;
  [], !/ [false; false] @> !/ [false; false] ;
]

let not_of_list = [
  [], !/ [] @> !/ [] ;
  [], !/ [false] @> !/ [true] ;
  [], !/ [true] @> !/ [false] ;
  [], !/ [false; false] @> !/ [true; true] ;
]

let list_and = [
  [], !/ [] @> RBase true ;
  [], !/ [true] @> RBase true ;
  [], !/ [false] @> RBase false ;
  [], !/ [true; false] @> RBase false ;
  [], !/ [false; false] @> RBase false ;
  [], !/ [false; true] @> RBase false ;
  [], !/ [true; true] @> RBase true ;
]

let list_or = [
  [], !/ [] @> RBase false ;
  [], !/ [true] @> RBase true ;
  [], !/ [false] @> RBase false ;
  [], !/ [true; false] @> RBase true ;
  [], !/ [false; true] @> RBase true ;
]

let list_cons = [
  [], RBase true @> !/ [] @> !/ [true] ;
  [], RBase false @> !/ [] @> !/ [false] ;
]

let list_append = [
  [], !/ [] @> !/ [] @> !/ [] ;
  [], !/ [] @> !/ [false] @> !/ [false] ;
  [], !/ [] @> !/ [true] @> !/ [true] ;
  [], !/ [true] @> !/ [] @> !/ [true] ;
  [], !/ [false] @> !/ [] @> !/ [false] ;
  [], !/ [true] @> !/ [false] @> !/ [true; false] ;
  [], !/ [false] @> !/ [false] @> !/ [false; false] ;
  [], !/ [true] @> !/ [true] @> !/ [true; true] ;
  [], !/ [false] @> !/ [true] @> !/ [false; true] ;
  (* [], !/ [false] @> !/ [false; false] @> !/ [false; false; false] ; *)
  [], !/ [false; true] @> !/ [true] @> !/ [false; true; true] ;
  [], !/ [true; true] @> !/ [true] @> !/ [true; true; true] ;
  (* [], !/ [true; false] @> !/ [false; false] @> !/ [true; false; false; false] ; *)
]

let test (name, worlds) =
  let _ = print_endline name in
  let start = Sys.time () in
  let res = synth worlds 20 in
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
    "bool impl", bool_impl ;
    "bool xor", bool_xor ;
    "list head", list_head ;
    "not list head", not_list_head ;
    "list tail", list_tail ;
    "list id", list_id ;
    "not of list", not_of_list ;
    "list and", list_and ;
    "list or", list_or ;
    "list cons", list_cons ;
    "list append", list_append ;
  ]

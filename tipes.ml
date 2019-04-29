type refinement =
  | RBase of bool
  | RArrow of refinement * refinement
  | RUnion of refinement * refinement
  | RIntersection of refinement * refinement
  | RProduct of refinement * refinement
  | RBottom
  | RCons of refinement * refinement
  | RNil
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
  | ECons of expr * expr
  | ENil
  | EMatch of expr * (expr * expr) list
  | EFix of string * string * expr

type context = (string * refinement) list
type world = context * refinement
type worlds = world list

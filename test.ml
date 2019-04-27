open Parse

let bool_id =
  let bool_ident_worlds = [
    [], RArrow (RBase true, RBase true) ;
    [], RArrow (RBase false, RBase false)
  ] in
  synth bool_ident_worlds 10

let bool_neg =
  let bool_neg_worlds = [
    [], RArrow (RBase false, RBase true) ;
    [], RArrow (RBase true, RBase false)
  ] in
  synth bool_neg_worlds 10

let bool_or =
  let bool_or_worlds = [
    [], RArrow (RBase false, RArrow (RBase true, RBase true)) ;
    [], RArrow (RBase false, RArrow (RBase false, RBase false)) ;
    [], RArrow (RBase true, RArrow (RBase false, RBase true))
  ] in
  synth bool_or_worlds 10

let bool_uncurry_and =
  let worlds = [
    [], RArrow (RProduct (RBase false, RBase true), RBase false) ;
    [], RArrow (RProduct (RBase true, RBase true), RBase true) ;
    [], RArrow (RProduct (RBase true, RBase false), RBase false)
  ] in
  synth worlds 10

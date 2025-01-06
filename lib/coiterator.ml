(** Coiteration 
    coiteration makes it possible to get rid of recursion.
    As a consequence, streams cannot be defined recursively as in
      constant v = cons v (constant v)
    Despite  the pre functions looks like cons, we cannot define
    constant this way because pre changes the type of the state.    
*)

(** The type of concrete streams *)

type ('a,'s) co = 
  | Co of ('s -> ('a * 's)) * 's 

let head (Co (h,s)) = fst (h s)
let tail (Co (h,s)) = snd (h s)

(* for testing purpose *)
let from_list (l : 'a list) (a :'a) : ('a, 'a list) co =
    Co ((fun l -> match l with [] -> (a,l) | h::t -> (h, t)), l)
  
(* let rec to_list (Co (f,i) : ('a,'b) co) (n : int) = 
  if n > 0 then 
    let (a, s') = f i in 
      a::(to_list (Co (f, s')) (n-1))
  else [] *)
    
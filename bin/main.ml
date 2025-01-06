open Format
open Fmt
open Ocamlfrp.Util
open Ocamlfrp.Coiterator
open Ocamlfrp.Yampa

(* STREAMS *)

(* stream of () *)

let dummy : (unit, unit) co = Co (dup, ())

(* stream of positive integers *)

let positives : (int, int) co = Co ((mapright ((+) 1) << dup), 0) 

(* STREAM FUNCTIONS WITHOUT LOOPS *)

let squares = arr dup >>> (arr (uncurry ( * )))

(* STREAM FUNCTIONS WITH LOOPS *)

let counter = loop (arr (mapright ((+) 1) << dup << snd)) 0
  
let pre = loop (arr swap)

let sum = loop (arr (dup << uncurry (+))) 0
  
(* References *)


type 'a cell = {content : 'a ref; access : (bool ref * bool ref) option}

type entry = E : 'a cell -> entry


let globals : entry list ref = ref []

let rec reset (l : entry list) =  
  match l with 
    | [] -> () 
    | (E {content = _ ;access = None})::t -> reset t
    | (E {content = _ ;access = Some (r1,r2)})::t -> r1 := true; r2 := true; reset t

let mkref x = 
  {content = ref x; access = None}

let mkglobalref x = 
  let r = {content = ref x; access = Some (ref true, ref true)}
  in globals := E r ::!globals; r 

exception IllegalRead 
exception IllegalWrite

let get : 'r cell -> ('a, 'x) co -> ('r * 'a, 'x) co =
  fun r (Co (h,x)) -> 
    match r.access with 
      | Some p ->
          Co ((fun x -> 
             if ! (fst p) then
              begin 
                fst p := false; 
                let (a,x') = h x in ((!(r.content), a), x')
              end
            else 
              raise IllegalRead), x) 
        | None -> Co ((fun x -> let (a,x') = h x in ((!(r.content), a), x')), x)
      


let set : 'r cell -> ('r * 'a, 'x) co -> ('a, 'x) co = 
    fun r (Co (h, x)) -> 
      match r.access with 
        | Some p ->
            Co ((fun x -> 
              if ! (snd p) then 
                begin
                  snd p := false;
                  let ((v,a), x') = h x in r.content := v; (a,x')
                end
              else 
                  raise IllegalWrite
              ), x)
        | None -> Co ((fun x -> let ((v,a), x') = h x in r.content := v; (a,x')), x)

let rec to_list (Co (f,i) : ('a,'b) co) (n : int) = 
  if n > 0 then 
    try 
      let (a, s') = f i in 
        begin 
        reset (!globals);
         a::(to_list (Co (f, s')) (n-1))
        end
      with 
        | IllegalRead -> Format.fprintf std_formatter "ILLEGAL READ\n"; []
        | IllegalWrite-> Format.fprintf std_formatter "ILLEGAL WRITE\n"; []
  else [] 
        

(* loop sf v = let r = mkref v in get r >>> f >>> set r, up to a swapping *)
(* but get r and set r may go deeper if not needed at all places *)
(* maybe we need to reverse input and register value in get and set *)

let counter_with_ref = 
  let r = mkref 0 in 
    get r >>> arr ((mapleft ((+) 1)) << dup << fst) >>> set r
    
let pref_with_ref v = 
  let r = mkref v in 
    get r >>> arr swap >>> set r

let sum_with_ref =
  let r = mkref 0 in 
    get r >>> arr (dup << uncurry (+)) >>> set r

let counter_with_ref_1 = 
  let r = mkglobalref 0 in 
    get r >>> arr ((mapleft ((+) 1)) << dup << fst) >>> set r
    
let counter_with_ref_2 = 
  let r = mkglobalref 0 in 
    get r >>> arr ((mapleft ((+) 1)) << dup << fst) >>> set r >>> get r 
    
(* catch exceptions *)

let _ = show (pp_print_int) (Some "positives:") (to_list positives 10)
let _ = show (pp_print_int) (Some "squares positives:") (to_list (squares positives) 10)
let _ = show (pp_print_int) (Some "counter:") (to_list (counter dummy) 10)
let _ = show (pp_print_int) (Some "pre positives:") (to_list (pre 0 positives) 10)
let _ = show (pp_print_int) (Some "sum positives:")(to_list (sum positives) 10)
let _ = show (pp_print_int) (Some "counter with ref:")(to_list (counter_with_ref dummy) 10)
let _ = show (pp_print_int) (Some "pre with ref:")(to_list (pref_with_ref 0 positives) 10)
let _ = show (pp_print_int) (Some "sum with ref:")(to_list (sum_with_ref positives) 10)
let _ = show (pp_print_int) (Some "counter with ref:")(to_list (counter_with_ref_1 dummy) 10)
let _ = show (pair pp_print_int pp_print_int) (Some "sum with ref:")(to_list (counter_with_ref_2 dummy) 10)

let _ = Format.fprintf std_formatter "done.\n"
    

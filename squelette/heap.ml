

type 'a t = 'a list (* 'a t is an ordered list *)

let empty = []

let is_singleton a =
   match a with
   [_] -> true 
   |_ -> false  

let is_empty a = 
  match a with 
  [] -> true 
  |_-> false

let rec add a1 a_liste = 
  match a_liste with 
  [] -> [a1]
  |e::suite -> if a1 <= e then (a1::a_liste) else e:: (add a1 suite) 

let find_min a_liste = 
match a_liste with 
[]->failwith "error "
|e::suite -> e


  let rec remove_min a_liste = 
  match a_liste with
  | [] -> failwith "error : list is empty"
  | e :: l -> e, l

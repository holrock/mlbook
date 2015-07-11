let rec map f l =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let rec calm = function
  | [] -> []
  | x::xs -> if x = '!'
	     then '.' :: calm xs
	     else x :: calm xs

let calm_map =
  map (fun x -> if x = '!' then '.' else x)

let rec apply f times init =
  if times <= 0 then init
  else apply f (times - 1) (f init)

let rec filter f = function
  | [] -> []
  | h::t -> if f h then h :: filter f t
	    else filter f t

let rec for_all f = function
  | [] -> true
  | h::t -> if f h then for_all f t else false

let rec mapl f = function
    | [] -> []
    | h::t -> map f h :: mapl f t

let _ =
  assert ((calm ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'])
	 = ['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.']);
  assert ((calm_map ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'])
	 = ['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.']);
  assert ((apply (fun n -> n  * 2) 3 2) = 16);
  assert ((filter (fun n -> n mod 2 = 0) [1; 2; 3; 4; 5]) = [2; 4])

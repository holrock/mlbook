let rec evens = function
  | _ :: h :: t -> h::evens t
  | _  -> []

let rec count_true ?(acc=0) = function
  | [] -> acc
  | true :: t -> count_true ~acc:(acc + 1) t
  | false :: t -> count_true ~acc:acc t

let rec palindrome ?(acc=[]) = function
  | [] -> acc
  | h::t -> palindrome ~acc:(h::acc) t

let rec drop_last ?(acc=[]) = function
  | [] -> []
  | _ :: [] ->  List.rev acc
  | h :: t -> drop_last ~acc:(h::acc) t

let rec member e = function
  | [] -> false
  | h :: _ when h == e -> true
  | _ :: t ->  member e t

let rec make_set ?(acc=[]) = function
  | [] -> List.rev acc
  | h::t ->
      if member h t  then make_set ~acc t
      else  make_set ~acc:(h::acc) t

let rec rev ?(acc=[])= function
  | [] -> acc
  | h :: t -> rev ~acc:(h::acc) t

assert ((evens [2; 4; 2; 4; 2]) = [4; 4])
assert ((count_true [true; false; true]) = 2)
assert ((palindrome ['h'; 'o'; 'g'; 'e']) = ['e'; 'g'; 'o'; 'h'])
assert ((drop_last [1; 2; 4; 8]) = [1; 2; 4])
assert ((member 2 [1; 2; 3]) = true)
assert ((member 3 [1; 2;]) = false)
assert ((make_set [1; 2; 3; 3; 1]) = [2; 3; 1])
assert ((rev [1;2;3]) = [3;2;1])

let rec merge x y =
  match x, y with
  | [], l | l, [] ->  l
  | hx::tx, hy::ty ->
      if hx < hy
      then hx :: merge tx y
      else hy :: merge x ty

let rec split ?(x=[]) ?(y=[]) = function
  | [] -> x, y
  | h :: t -> split ~x:(h :: y) ~y:x t

let rec msort = function
  | [] -> []
  | [_] as x -> x
  | l ->
      let left, right = split l in
      merge (msort left) (msort right)

let rec insert comp x = function
  | [] -> [x]
  | h :: t ->
      if comp x h
      then x :: h :: t
      else h :: insert comp x t

let rec isort comp = function
  | [] -> []
  | h :: t -> insert comp h (isort comp t)

let sort = isort (fun x y -> x <= y)
let rsort = isort (fun x y -> x >= y)

let rec is_sorted = function
  | [] | [_] -> true
  | x :: (y :: _ as rest) -> if x > y then false
      else is_sorted rest

let rec comb_sort = function
  | [] -> []
  | h :: t ->
      match comb_sort t with
      | [] -> [h]
      | th :: tt as rest->
          if h <= th
          then h :: rest
          else th :: comb_sort (h :: tt)

let _ =
  assert ((msort [6; 4; 5; 7; 2; 5; 3; 4]) = [2; 3; 4; 4; 5; 5; 6; 7]);
  assert ((sort [53; 9; 2; 6; 19]) = [2; 6; 9; 19; 53]);
  assert ((rsort [53; 9; 2; 6; 19]) = [53; 19; 9; 6; 2]);
  assert (is_sorted [1;2;3]);
  assert (is_sorted [1;3;2] = false);
  assert ((comb_sort [53; 9; 2; 6; 19]) = [2; 6; 9; 19; 53]);

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

let _ =
  assert ((msort [6; 4; 5; 7; 2; 5; 3; 4]) = [2; 3; 4; 4; 5; 5; 6; 7])

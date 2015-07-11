let rec smallest = function
  | [] -> raise Not_found
  | [x] -> x
  | h::m::t -> if h < m then smallest (h::t)
	       else smallest (m::t)

let rec smallest_or_zero list =
  try smallest list with Not_found -> 0

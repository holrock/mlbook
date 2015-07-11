let rec replace k v = function
  | [] -> raise Not_found
  | (k', v')::t ->
     if k' = k
     then (k, v) :: t
     else replace k v t

let rec zip a b =
  match a, b with
  | [], [] -> []
  | _, [] | [], _ -> raise (Invalid_argument "error")
  | ah::at, bh::bt ->
     (ah, bh) :: zip at bt

let rec unzip = function
  | [] -> [], []
  | (k, v)::t ->
     let k', v' = unzip t in
     (k :: k', v :: v')

let rec dictonary_of_list = function
  | [] -> []
  | (k, v)::t ->
     (k, v) :: (dictonary_of_list
		  (List.filter (fun (k', _) -> k <> k') t))

let rec member k = function
	  | [] -> false
	  | (k', _):: t -> if k = k' then true
			   else member k t

let union a b =
  let rec f = function
    | [] -> a
    | (k,v)::bt -> if member k a
		then f bt
		else  (k,v) :: (f bt)
  in
  f b

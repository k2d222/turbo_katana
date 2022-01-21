let index_of_opt v l =
  let rec r_index l i =
    match l with
    | [] -> None
    | x::_ when x = v -> Some(i)
    | _::r -> r_index r (i+1)
  in r_index l 0

let index_of v l =
  match index_of_opt v l with
  | Some(v) -> v
  | None -> raise Not_found

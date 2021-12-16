open Optmanip

let find_pairs f l =
  let rec r_find l =
    match l with
      | [] -> None
      | a1::r -> List.find_opt (fun a2 -> f (a1, a2)) r |> map (fun a2 -> (a1, a2))
  in r_find l

let iter_pairs f l =
  ignore (find_pairs (fun pair -> f pair; false) l)

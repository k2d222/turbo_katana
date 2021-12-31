open Optmanip

let find_pairs f l =
  let rec r_find l =
    match l with
    | [] -> None
    | a1::r -> List.find_opt (fun a2 -> f (a1, a2)) r |> map (fun a2 -> (a1, a2))
  in r_find l

let iter_pairs f l =
  ignore (find_pairs (fun pair -> f pair; false) l)


module Env = struct
  type t = (string * string) list

  (** Add a variable to the environment, shadowing any pre-existing variable. *)

  let add env (var: Ast.param) =
    let env = List.remove_assoc var.name env
    in (var.name, var.className) :: env

  (** Add all variables to the environment, shadowing any pre-existing variable. *)

  let add_all env vars =
    vars |> List.fold_left add env

  (** Get a variable from the environment.
      @raise Not_found if the variable is not in the environment. *)

  let get env varName =
    List.assoc_opt varName env
    |> get_or_else (fun () ->
        Printf.eprintf "[ERR] env_get '%s' failed\n" varName;
        raise Not_found
      )

  let print env =
    env |> List.iter (fun (name, type_) ->
        Printf.printf "%s : %s\n" name type_
      )
end

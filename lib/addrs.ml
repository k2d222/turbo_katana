open Optmanip

type t = (string * int) list

(** Get the address (offset from fp) of a local variable.
    @raise Not_found if the variable is not in the environment. *)

let get addrs id =
  List.assoc_opt id addrs
  |> get_or_else (fun () ->
      Printf.eprintf "[ERR] addrs_get '%s' failed\n" id;
      raise Not_found
    )

(** Add a local variable to the address lookup.
    The address will be the top of the stack of local variables. *)

let add_local addrs name =
	let prev = List.fold_left (fun acc (_name, addr) -> max acc addr) (-1) addrs
	in (name, prev + 1)::addrs 
  
(** Create the address lookup for a method call.
    Order of stack: result, params, this *)

let make_method_addrs params =
  let len = List.length params
  in let addrs = params |> List.mapi (fun i (p: Ast.param) -> 
      (p.name, -len - 1 + i)
    )
  in let addrs = ("result", -len - 2)::addrs
  in let addrs = ("this", -1)::addrs
  in addrs

(** Create the address lookup for a static method call.
    Note: order of stack: result, params *)

let make_static_method_addrs params =
  let len = List.length params
  in let addrs = params |> List.mapi (fun i (p: Ast.param) -> 
      (p.name, -len + i)
    )
  in let addrs = ("result", -len - 1)::addrs
  in addrs

(** Create the address lookup for a constructor call.
    Note: order of stack: this, params *)

let make_ctor_addrs params =
  let len = List.length params
  in let addrs = params |> List.mapi (fun i (p: Ast.param) -> 
      (p.name, -len + i)
    )
  in let addrs = ("this", -len - 1)::addrs
  in addrs

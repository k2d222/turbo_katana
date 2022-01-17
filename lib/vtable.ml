open Ast
open Astmanip

(** Get the offset of a method in the VTable
    @raise Not_found if method is not in vtable *)

let offset vt (meth: methodDecl) =
  let rec r_find i vt = 
    match vt with
    | [] -> raise Not_found
    | (name, _)::_ when name = meth.name -> i
    | _::r -> r_find (i+1) r
  in r_find 0 vt
  
(** Insert a method in the VTable *)

let insert vt methName decl = 
  let rec r_repl vt =
    match vt with
    | [] -> [(methName, decl)]
    | (name, _)::r when name = methName -> (methName, decl)::r
    | m::r -> m::r_repl r
  in r_repl vt

(** Build a VTable from a class declaration *)

let rec make decls decl = 
  match decl.superclass with
  | Some(super) ->
      let super = get_class decls super 
      in let vt = make decls super
      in decl.body.instMethods
      |> List.fold_left (fun vt (m: methodDecl) -> 
          insert vt m.name decl
        ) vt

  | None -> 
      decl.body.instMethods 
      |> List.map (fun (m: methodDecl) -> 
          (m.name, decl)
        )


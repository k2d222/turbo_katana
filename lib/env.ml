open Optmanip

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

(** Make an environment with 'super' and 'this'. *)

let make_class_env (decl: Ast.classDecl) =
  let env = ("this", decl.name) :: []
  in let env = match decl.super with
      | Some(super) -> ("super", super.name) :: env
      | None -> env
  in env

(** Make an environment with method params and optionally 'result'. *)

let add_method_env env (meth: Ast.methodDecl) =
  let env = add_all env meth.params
  in let env = match meth.retType with
      | Some(ret) -> ("result", ret) :: env
      | None -> env
  in env

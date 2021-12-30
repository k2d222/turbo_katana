open Ast

exception Contextual_error of string

(** Find the class declaration with a given name. *)

let find_class_opt name decls =
  List.find_opt (fun decl -> decl.name = name) decls

(** Find the class declaration with a given name.
    @raise Not_found if no such declaration is found. *)

let find_class name decls =
  List.find (fun decl -> decl.name = name) decls

(** List of all ancestor class declarations in bottom-to-top order.
    @raise Not_found if an ancestor has no declaration. *)

let rec ancestors decl decls =
  match decl.superclass with
  | None -> []
  | Some(super) ->
    let superDecl = (find_class super decls) in
    superDecl :: (ancestors superDecl decls)

(** Tells if a class extends a base class (recursively)
    @raise Not_found if a class name has no declaration. *)

let is_superclass base super decls =
  let superDecl = find_class super decls in
  List.find_opt ((=) base) (ancestors superDecl decls)
  |> Option.is_some


(** Get the type of an attribute in a class declaration. *)

let get_attr_type decl attrName =
  decl.body.instAttrs
  |> List.find_map  (fun (attr: param) ->
      if attr.name = attrName  then Some(attr.className)  else None
    )
  |> Option.get

(** Get the type of a method in a class declaration. *)

let get_method_type decl methName =
  decl.body.methods
  |> List.find_map  (fun (meth: methodDecl) ->
      if meth.name = methName then meth.retType else None
    )
  |> Optmanip.get_or("Void")

(** Computes an expression type. *)

let get_expr_type env decls expr =
  let rec r_get expr =
    match expr with
    | Cste _ | BinOp _ | UMinus _ -> "Integer"
    | String _ | StrCat _ -> "String"

    | Id id -> List.assoc id env

    | AttrOf(e, attrName) ->
      let decl = find_class (r_get e) decls
      in get_attr_type decl attrName

    | List l ->
      let last = List.hd (List.rev l)
      in r_get last

    | MethodCall(name, caller, _args) ->
      let decl = find_class (r_get caller) decls
      in get_method_type decl name

    | New(className, _args) -> className

  in r_get expr

(* -------------------------------------------------------------------------- *)

(** Check constructor declaration validity. Performs following checks:
    - Constructor name and class name are equal
    - Constructor parameters and class parameters are equal
    - Constructor calls the right super constructor if class is derived
    - Constructor does not call any super constructor if class is base
      @raise Contextual_error if a check fails.
*)

let check_ctor decl =
  let ctor = decl.body.ctor in

  if decl.name <> ctor.name
  then raise @@ Contextual_error (Printf.sprintf "constructor name '%s' does dot correspond with class name '%s'" ctor.name decl.name)
  else ();

  match decl.superclass, ctor.superCall with
  | Some(n1), Some(n2, _) when n1 <> n2 -> raise @@ Contextual_error (Printf.sprintf "class '%s' extends superclass '%s' but constructor calls super constructor of '%s'" decl.name n1 n2)
  | Some(n1), None -> raise @@ Contextual_error (Printf.sprintf "class '%s' extends superclass '%s' but constructor does not call the super constructor" decl.name n1)
  | None, Some(n2, _) -> raise @@ Contextual_error (Printf.sprintf "class '%s' is a base class but constructor calls super constructor of '%s'" decl.name n2)
  | _ -> ();

    if ctor.params <> decl.ctorParams
    then raise @@ Contextual_error (Printf.sprintf "constructor params of class '%s' do not correspond with the constructor definition" decl.name)
    else ()

(** Check that each method declaration is unique in a class declaration.
    @raise Contextual_error if a check fails. *)

let check_multiple_def decl =
  Util.iter_pairs (fun ((d1: methodDecl), (d2: methodDecl)) ->
      if d1.name = d2.name
      then raise @@ Contextual_error (Printf.sprintf "multiple definition of method '%s' in class '%s'" d1.name decl.name)
      else ()
    ) decl.body.methods

(** Check that each class declaration extends a declared class, if any.
    @raise Contextual_error if a check fails. *)

let check_inheritance decls =
  List.filter_map (fun d -> d.superclass |> Optmanip.map (fun super -> (d.name, super))) decls
  |> List.iter (fun (name, super) ->
      match find_class_opt super decls with
      | None -> raise @@ Contextual_error (Printf.sprintf "class '%s' extends non-existing class '%s'" name super)
      | _ -> ()
    )

(** Check that there are no cycles in the inheritance graph.
    @raise Contextual_error if a check fails. *)

let check_cycles decls =
  (* complexity unoptimized (add memoization?) *)
  let rec r_check ancestors decl =
    match decl.superclass with
    | Some(super) ->
      let superDecl = find_class_opt super decls |> Option.get in
      if List.exists ((=) super) ancestors
      then raise @@ Contextual_error (Printf.sprintf "cycle in heritance: class '%s' extends ancestor class '%s'" decl.name super)
      else r_check (super::ancestors) superDecl
    | None -> ()
  in List.iter (r_check []) decls

(** Performs the following checks:
    - Override methods have the 'override' keyword.
    - Override methods match the overriden method signature.
      @raise Contextual_error if a check fails. *)

let check_overrides decls =
  () (* TODO *)

(** Checks that an expression is valid with the given scope.
    @raise Contextual_error if a check fails. *)

let rec check_expr_scope env expr =
  () (* TODO *)

(** Performs the following checks:
    - The method exists for the given type
    - Method call parameters are compatible with the declaration.
      @raise Contextual_error if a check fails. *)

let rec check_method_calls env expr instr =
  () (* TODO *)

(** Performs the following checks:
    - Left-hand-side assign operand refers to either:
      (a) A variable in the scope, or
      (b) (recusively) an attribute of a variable in the scope, or
      (c) A static attribute of a class.
    - Right-hand-side assign operand is compatible with the target variable
      @raise Contextual_error if a check fails. *)

let rec check_assign env (lhs, rhs) =
  () (* TODO *)

(** Performs the following checks:
    - All code paths lead to either:
      (a) A return, or
      (b) an assign to the implicit 'result' variable.
    - All return instructions have a type compatible with the return type *)

let check_returns retType instr =
  let rec r_check env instr =
    () (* TODO *)
  in r_check [("result", retType)] instr

(** Checks that there are no return instruction.
    @raise Contextual_error if a check fails. *)

let rec check_no_return instr =
  () (* TODO *)

(** Checks that an if/then/else instruction is valid.
    @raise Contextual_error if a check fails. *)

let check_ite_instr (if_, then_, else_) =
  () (* TODO *)

(* -------------------------------------------------------------------------- *)

let check_main_instr instr decls =
  check_no_return instr;
  () (* TODO *)

let check_decl decl =
  check_ctor decl;
  check_multiple_def decl;
  () (* TODO *)

let check_decls decls =
  check_inheritance decls;
  check_cycles decls;
  check_overrides decls;
  List.iter check_decl decls;
  () (* TODO *)

(** Perform all checks on ast.
    @raise Contextual_error if a check fails. *)

let check_all ast =
  check_decls ast.decls;
  check_main_instr ast.instr ast.decls

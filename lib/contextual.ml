open Ast

exception Contextual_error of string

(** Find the class declaration with a given name. *)

let find_class_opt decls name =
  List.find_opt (fun decl -> decl.name = name) decls

(** Find the class declaration with a given name.
    @raise Not_found if no such declaration is found. *)

let find_class decls name =
  List.find (fun decl -> decl.name = name) decls

(** List of all ancestor class declarations in bottom-to-top order.
    @raise Not_found if an ancestor has no declaration. *)

let rec ancestors decls decl =
  match decl.superclass with
  | None -> []
  | Some(super) ->
    let superDecl = (find_class decls super) in
    superDecl :: (ancestors decls superDecl)

(** Find (recursively through ancestors) the method declaration in a class
    with a given name. *)

let rec find_method_opt decls name decl =
  List.find_opt (fun (meth: methodDecl) -> meth.name = name) decl.body.methods
  |> Optmanip.or_else (fun () ->
      match decl.superclass with
      | None -> None
      | Some(super) ->
        let superDecl = (find_class decls super)
        in find_method_opt decls name superDecl
    )


(** Get the type of an attribute in a class declaration. *)

let get_attr_type attrName decl =
  decl.body.instAttrs
  |> List.find_map  (fun (attr: param) ->
      if attr.name = attrName  then Some(attr.className)  else None
    )
  |> Option.get

(** Get the type of a method in a class declaration. *)

let get_method_type methName decl =
  decl.body.methods
  |> List.find_map  (fun (meth: methodDecl) ->
      if meth.name = methName then meth.retType else None
    )
  |> Optmanip.get_or("Void")

(** Computes an expression type. *)

let get_expr_type decls env expr =
  let rec r_get expr =
    match expr with
    | Cste _ | BinOp _ | UMinus _ -> "Integer"
    | String _ | StrCat _ -> "String"

    | Id id -> List.assoc id env

    | Attr(e, attrName) ->
      let decl = find_class decls (r_get e)
      in get_attr_type attrName decl

    | StaticAttr(className, attrName) ->
      let decl = find_class decls className
      in get_attr_type attrName decl

    | List l ->
      let last = List.hd (List.rev l)
      in r_get last

    | Call(caller, name, _args) ->
      let decl = find_class decls (r_get caller)
      in get_method_type name decl

    | StaticCall(className, name, _args) ->
      let decl = find_class decls className
      in get_method_type name decl

    | New(className, _args) -> className

  in r_get expr

(** Wether derived is convertible to base. *)

let is_base decls derived base =
  if derived = base then true
  else
    let derived = find_class decls derived
    in let base = find_class decls base
    in List.exists ((=) base) (ancestors decls derived)

let add_to_env env vars =
  vars |> List.fold_left (fun env (var: param) ->
      let env = List.remove_assoc var.name env
      in (var.name, var.className) :: env
    ) env

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
      match find_class_opt decls super with
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
      let superDecl = find_class_opt decls super |> Option.get in
      if List.exists ((=) super) ancestors
      then raise @@ Contextual_error (Printf.sprintf "cycle in heritance: class '%s' extends ancestor class '%s'" decl.name super)
      else r_check (super::ancestors) superDecl
    | None -> ()
  in List.iter (r_check []) decls

(** Performs the following checks:
    - Base classes cannot have override methods.
    - Override methods have the 'override' keyword.
    - Override methods match the overriden method signature.
      @raise Contextual_error if a check fails. *)

let check_overrides decls =

  let check_class decl =

    let check_super_method superDecl (meth: methodDecl) =
      let overriden = find_method_opt decls meth.name superDecl in
      if meth.override then
        match overriden with
        | Some(overriden) ->
          if meth.params <> overriden.params
          then raise @@ Contextual_error (Printf.sprintf "signature mismatch between method '%s::%s' and overriden method" decl.name meth.name)
          else ()
        | None -> raise @@ Contextual_error (Printf.sprintf "method '%s::%s' is marked override but no overriden method found" decl.name meth.name)
      else
        match overriden with
        | Some _ -> raise @@ Contextual_error (Printf.sprintf "method '%s::%s' is not marked override but shadows a super method" decl.name meth.name)
        | None -> ()

    in let check_base_method (meth: methodDecl) =
         if meth.override
         then raise @@ Contextual_error (Printf.sprintf "method '%s' of base class '%s' is marked override" meth.name decl.name)
         else ()

    in match decl.superclass with
    | Some(super) ->
      let superDecl = find_class decls super
      in List.iter (check_super_method superDecl) decl.body.methods
    | None -> List.iter check_base_method decl.body.methods

  in List.iter check_class decls

(** Checks that all identifiers in an expression are in scope.
    @raise Contextual_error if a check fails. *)

let check_expr_ident_scope env expr =
  let rec r_check expr =
    match expr with
    | Id id ->
      if Option.is_none @@ List.assoc_opt id env
      then raise @@ Contextual_error (Printf.sprintf "use of undeclared identifier '%s'" id)
      else ()
    | Attr(e, _) | UMinus e | Call(e, _, _) -> r_check e
    | List le | New(_, le) -> List.iter r_check le
    | BinOp(e1, _, e2) | StrCat(e1, e2) -> r_check e1; r_check e2
    | Cste _ | StaticAttr _ | StaticCall _ | String _ -> ()

  in r_check expr

(** Checks that all identifiers in an instruction are in scope.
    @raise Contextual_error if a check fails. *)

let rec check_instr_ident_scope env instr =
  let rec r_check instr =
    match instr with
    | Block(vars, li) ->
      let env = add_to_env env vars
      in List.iter (check_instr_ident_scope env) li

    | Assign(to_, from_) ->
      check_expr_ident_scope env to_;
      check_expr_ident_scope env from_;

    | Return e | Expr e -> check_expr_ident_scope env e

    | Ite(e, then_, else_) ->
      check_expr_ident_scope env e;
      r_check then_; r_check else_

  in r_check instr

(** Performs the following checks:
    - The method exists for the given type
    - Method call parameters are compatible with the declaration.
      @raise Contextual_error if a check fails. *)

let rec check_method_calls _env _expr _instr =
  () (* TODO *)

(** Performs the following checks:
    - Left-hand-side assign operand refers to either:
      (a) An ident,
      (b) (recusively) an attribute of a variable, or
      (c) A static attribute of a class.
    - Right-hand-side assign operand is compatible with the target variable
      @raise Contextual_error if a check fails. *)

let rec check_assign decls env (lhs, rhs) =
  let t1 = get_expr_type decls env lhs
  in let t2 = get_expr_type decls env rhs
  in let () = match lhs with
      | Id _ | Attr _ | StaticAttr _ -> ()
      | _ -> raise @@ Contextual_error (Printf.sprintf "cannot assign to an expression of type '%s'" t1)
  in let () =
       if is_base decls t1 t2
       then ()
       else raise @@ Contextual_error (Printf.sprintf "cannot assign '%s' to '%s'" t2 t1);
  in ()

(** Performs the following checks:
    - All code paths lead to either:
      (a) A return, or
      (b) an assign to the implicit 'result' variable.
    - All return instructions have a type compatible with the return type
    - All assigns to result have a type compatible with the return type *)

let check_returns decls retType instr =

  let assert_type env e =
    let t = get_expr_type decls env e
    in if is_base decls t retType
    then ()
    else raise @@ Contextual_error (Printf.sprintf "invalid return type '%s', expected type compatible with '%s'" t retType);

  in let rec check_ret_type env instr =
       match instr with
       | Return e -> assert_type env e

       | Block(vars, li) ->
         let env = add_to_env env vars
         in List.iter (check_ret_type env) li

       | Ite(_, then_, else_) ->
         check_ret_type env then_;
         check_ret_type env else_

       | Expr _ | Assign _ -> () (* assigns are already tested in check_assign *)

  in let rec has_return instr =
       match instr with
       | Block(_, li) -> List.exists has_return li
       | Return _ -> true
       | Assign(to_, _) when to_ = Id("result") -> true
       | Ite(_, then_, else_) -> has_return then_ && has_return else_
       | _ -> false

  in let () = check_ret_type [] instr
  in let () = if not (has_return instr)
       then raise @@ Contextual_error (Printf.sprintf "some code paths lead to no return statement or assign to 'result' when method expects return type '%s'" retType);
  in ()

(** Checks that there are no return instruction.
    @raise Contextual_error if a check fails. *)

let rec check_no_return instr =
  match instr with
  | Return _ -> raise @@ Contextual_error (Printf.sprintf "no return instruction are allowed here");
  | Block(_, li) -> List.iter check_no_return li
  | Ite(_, then_, else_) -> check_no_return then_; check_no_return else_
  | Expr _ | Assign _ -> ()

(** Checks that an if/then/else instruction is valid.
    @raise Contextual_error if a check fails. *)

let check_ite_instr (_if_, _then_, _else_) =
  () (* TODO *)

(* -------------------------------------------------------------------------- *)

let check_main_instr _decls instr =
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
  check_main_instr ast.decls ast.instr

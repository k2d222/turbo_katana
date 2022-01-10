open Ast
open Astmanip
open Util

exception Contextual_error of string

let err str = raise (Contextual_error str)
(* let err str = print_endline @@ "[CONTEXTUAL ERROR]: " ^ str; () *)

(** Check that methods, instance attributes and static attributes unique in a class declaration.
    @raise Contextual_error if a check fails. *)

let check_no_dup decl =
  let instMethods = decl.body.instMethods |> List.map (fun (m: methodDecl) -> m.name)
  in let staticMethods = decl.body.staticMethods |> List.map (fun (m: methodDecl) -> m.name)
  in let staticAttrs = decl.body.staticAttrs |> List.map (fun (a: param) -> a.name)
  in let instAttrs = decl.body.instAttrs |> List.map (fun (a: param) -> a.name)

  in let rec check t = function
      | [] -> ()
      | e::r ->
        if List.exists ((=) e) r
        then err (Printf.sprintf "multiple definition %s of '%s' in class '%s'" t e decl.name)
        else check t r

  in check "method" instMethods;
  check "static method" staticMethods;
  check "attribute" instAttrs;
  check "static attribute" staticAttrs

(** Check that each class declaration extends a declared class, if any.
    @raise Contextual_error if a check fails. *)

let check_inheritance decls =
  let decls_with_super = decls |> List.filter_map (fun d ->
      d.superclass |> Optmanip.map (fun super -> (d.name, super))
    )
  in decls_with_super |> List.iter (fun (name, super) ->
      match get_class_opt decls super with
      | None -> err (Printf.sprintf "class '%s' extends unknown class '%s'" name super)
      | _ -> ()
    )

(** Check that there are no cycles in the inheritance graph.
    @raise Contextual_error if a check fails. *)

let check_cycles decls =
  (* complexity unoptimized (add memoization?) *)
  let rec r_check ancestors decl =
    match decl.superclass with
    | Some(super) ->
      let superDecl = get_class_opt decls super |> Option.get in
      if List.exists ((=) super) ancestors
      then err (Printf.sprintf "cycle in heritance: class '%s' extends ancestor class '%s'" decl.name super)
      else r_check (super::ancestors) superDecl
    | None -> ()
  in List.iter (r_check []) decls

(** Performs the following checks:
    - Base classes cannot have override methods.
    - Override methods have the 'override' keyword.
    - Override methods match the overriden method signature.
      @raise Contextual_error if a check fails. *)

let check_overrides decls decl =

  let check_params derived base =
    if List.length derived.params <> List.length base.params
    then err (Printf.sprintf "parameters of override method '%s::%s' do not correspond with overriden method" decl.name derived.name)
    else List.iter2 (fun (p1: param) (p2: param) ->
        if not (p1.className = p2.className)
        then err (Printf.sprintf "parameter '%s' in method '%s::%s' must be of type '%s' to match overriden method" p1.name decl.name derived.name p2.className)
      ) derived.params base.params

  in let check_super_method superDecl (meth: methodDecl) =
       let overriden = find_method_opt decls meth.name superDecl in
       if meth.override then
         match overriden with
         | Some(overriden) ->
           check_params meth overriden;
           if meth.params <> overriden.params
           then err (Printf.sprintf "signature mismatch between method '%s::%s' and overriden method" decl.name meth.name)
           else ()
         | None -> err (Printf.sprintf "method '%s::%s' is marked override but no overriden method found" decl.name meth.name)
       else
         match overriden with
         | Some _ -> err (Printf.sprintf "method '%s::%s' is not marked override but shadows a super method" decl.name meth.name)
         | None -> ()

  in let check_base_method (meth: methodDecl) =
       if meth.override
       then err (Printf.sprintf "method '%s' of base class '%s' is marked override" meth.name decl.name)
       else ()

  in match decl.superclass with
  | Some(super) ->
    let superDecl = get_class decls super
    in List.iter (check_super_method superDecl) decl.body.instMethods
  | None -> List.iter check_base_method decl.body.instMethods

(** Checks that id is in scope.
    @raise Contextual_error if a check fails. *)

let check_in_scope env id =
  if Option.is_none @@ List.assoc_opt id env
  then err (Printf.sprintf "use of undeclared identifier '%s'" id)

(** Performs the following checks:
    - The method exists for the given type
    - Method call parameters are compatible with the declaration.
      @raise Contextual_error if a check fails. *)

let check_method_calls _env _expr _instr =
  () (* TODO *)

(** Performs the following checks:
    - All code paths lead to either an assign to the implicit 'result' variable before return instruction. *)

let check_returns instr =
  let rec has_result instr =
       match instr with
       | Block(_, li) -> 
          li |> List.fold_left (fun (hasRet, res) i -> 
            if hasRet then (true, false)
            else if res then (false, true)
            else match i with
               | Return -> (true, false)
               | _ -> (false, has_result i)
          ) (false, false)
          |> snd
       | Assign(to_, _) when to_ = Id("result") -> true
       | Ite(_, then_, else_) -> has_result then_ && has_result else_
       | _ -> false

  in if not (has_result instr)
  then err (Printf.sprintf "some code paths lead to no assign to 'result' before end of block or return statement, when method expects a return")

(** Checks that there are no declarations of reserved keywords in vars.
    Note: reserved keywords are: 'this', 'super', 'result'.
    @raise Contextual_error if a check fails. *)

let check_no_reserved_var vars =
  let reserved = ["this"; "super"; "result"]

  in let check (var: param) =
       if List.exists ((=) var.name) reserved
       then err (Printf.sprintf "use of reserved keyword '%s'" var.name)

  in List.iter check vars

(** Checks that there are no class declarations with reserved name (String or Integer).
    @raise Contextual_error if a check fails. *)

let check_no_reserved_class decls =
  let reserved = ["String"; "Integer"; "_Void"]

  in let check decl =
       if List.exists (fun r -> decl.name = r || decl.superclass = Some(r)) reserved
       then err (Printf.sprintf "use of reserved class in class '%s'" decl.name)

  in List.iter check decls

(** Checks that there are no duplicate class declarations.
    @raise Contextual_error if a check fails. *)

let rec check_no_dup_class = function
  | [] -> ()
  | decl :: decls ->
    if List.exists (fun other -> decl.name = other.name) decls
    then err (Printf.sprintf "duplicate class declaration: '%s'" decl.name)
    else check_no_dup_class decls

(** Check constructor declaration validity. Performs following checks:
    * Constructor name and class name are equal
    * Constructor parameters and class parameters are equal
    * Constructor parameters have no reserved keywords
    * Constructor calls the right super constructor if class is derived
    * Constructor does not call any super constructor if class is base
    * No return instruction in body
    @raise Contextual_error if a check fails.
*)

let check_ctor decl =
  let ctor = decl.body.ctor
  in let params = ctor.params |> List.map (fun { name; className; _ } -> { name; className })
  in begin
    check_no_reserved_var params;

    if decl.name <> ctor.name
    then err (Printf.sprintf "constructor name '%s' does dot correspond with class name '%s'" ctor.name decl.name)
    else ();

    (match decl.superclass, ctor.superCall with
     | Some(n1), Some(n2, _) when n1 <> n2 -> err (Printf.sprintf "class '%s' extends superclass '%s' but constructor calls super constructor of '%s'" decl.name n1 n2)
     | Some(n1), None -> err (Printf.sprintf "class '%s' extends superclass '%s' but constructor does not call the super constructor" decl.name n1)
     | None, Some(n2, _) -> err (Printf.sprintf "class '%s' is a base class but constructor calls super constructor of '%s'" decl.name n2)
     | _ -> ());

    if ctor.params <> decl.ctorParams
    then err (Printf.sprintf "constructor params of class '%s' do not correspond with the constructor definition" decl.name)
    else ()
  end

(** Check compatible call arguments *)

let check_call_args decls args meth =

  let check_arg arg param =
    if not (is_base decls arg param)
    then err (Printf.sprintf "invalid call argument: type '%s' is incompatible with '%s'" arg param)

  in if List.length args <> List.length meth.params
  then err (Printf.sprintf "invalid number of arguments in call to method '%s'" meth.name);

  List.iter2 (fun arg (param: param) ->
      check_arg arg param.className
    ) args meth.params

(** Checks that Block instructions are valid.
    @raise Contextual_error if a check fails. *)

let rec check_instr_block decls env (vars, li) =
  check_no_reserved_var vars;
  let env = Env.add_all env vars
  in List.iter (check_instr decls env) li

(** Performs the following checks:
    - Left-hand-side assign operand refers to either:
      (a) An ident,
      (b) (recusively) an attribute of a variable, or
      (c) A static attribute of a class.
    - Right-hand-side assign operand is compatible with the target variable
      @raise Contextual_error if a check fails. *)

and check_instr_assign decls env (lhs, rhs) =
  check_expr decls env lhs;
  check_expr decls env rhs;
  let t1 = get_expr_type decls env lhs
  in let t2 = get_expr_type decls env rhs
  in let () = match lhs with
      | Id _ | Attr _ | StaticAttr _ -> ()
      | _ -> err (Printf.sprintf "cannot assign to an expression of type '%s'" t1)
  in let () =
       if is_base decls t2 t1
       then ()
       else err (Printf.sprintf "cannot assign '%s' to '%s'" t2 t1)
  in ()

(** Checks that if/then/else instructions are valid.
    @raise Contextual_error if a check fails. *)

and check_instr_ite decls env (e, then_, else_) =
  check_expr decls env e;
  let t = get_expr_type decls env e
  in if t <> "Integer"
  then err (Printf.sprintf "'if' condition must be of type 'Integer'");
  check_instr decls env then_;
  check_instr decls env else_

(** Checks an instruction.
    @raise Contextual_error if a check fails. *)

and check_instr decls env instr =
  match instr with
  | Block (vars, li) -> check_instr_block decls env (vars, li)
  | Assign (to_, from_) -> check_instr_assign decls env (to_, from_)
  | Ite (e, then_, else_) -> check_instr_ite decls env (e, then_, else_)
  | Expr e -> check_expr decls env e
  | Return -> ()

(** Checks an Attr expression.
    @raise Contextual_error if a check fails. *)

and check_expr_attr decls env (e, name) =
  check_expr decls env e;
  let t = get_expr_type decls env e
  in let decl = get_class decls t
  in let attr = find_inst_attr_opt decls name decl
  in if Option.is_none attr
  then err (Printf.sprintf "no attribute named '%s' in class '%s'" name t)

(** Checks an StaticAttr expression.
    @raise Contextual_error if a check fails. *)

and check_expr_static_attr decls (t, name) =
  let decl = get_class decls t
  in let attr = get_static_attr_opt name decl
  in if Option.is_none attr
  then err (Printf.sprintf "no static attribute named '%s' in class '%s'" name t)

(** Checks a function call expression.
    @raise Contextual_error if a check fails. *)

and check_expr_call decls env (e, methName, args) =
  check_expr decls env e;
  let t = get_expr_type decls env e

  in match t, methName, args with
  | "String", "print", []
  | "String", "println", []
  | "Integer", "toString", [] -> ()

  | "Integer", _, _::_
  | "String", _, _::_ -> err (Printf.sprintf "'%S::%s' expects no arguments" t methName)

  | "_Void", _, _ -> err (Printf.sprintf "call to method '%s' of void expression" t)

  | "String", _, _
  | "Integer", _, _ -> err (Printf.sprintf "call to unknown method '%s::%s'" t methName)

  | _ ->
    let decl = get_class decls t
    in let meth = find_method_opt decls methName decl
    in let args = args |> List.map (fun e ->
        check_expr decls env e;
        get_expr_type decls env e
      )
    in match meth with
    | Some(meth) -> check_call_args decls args meth
    | None -> err (Printf.sprintf "call to unknown method '%s::%s'" t methName)

(** Checks a function call expression.
    @raise Contextual_error if a check fails. *)

and check_expr_static_call decls env (className, methName, args) =
  let decl = get_class_opt decls className
  in match decl with
  | None -> err (Printf.sprintf "call to static method '%s' of unknown class '%s'" methName className)
  | Some(decl) ->
    let meth = get_static_method_opt methName decl
    in let args = args |> List.map (fun e ->
        check_expr decls env e;
        get_expr_type decls env e
      )
    in match meth with
    | Some(meth) -> check_call_args decls args meth
    | None -> err (Printf.sprintf "call to unknown static method '%s::%s'" className methName)

(** Checks a New expression.
    @raise Contextual_error if a check fails. *)

and check_expr_new decls env (className, args) =
  let decl = get_class_opt decls className
  in match decl with
  | None -> err (Printf.sprintf "instantiation of unknown class '%s'" className)
  | Some(decl) ->
    let args = args |> List.map (fun e ->
        check_expr decls env e;
        get_expr_type decls env e
      )

    in let check_arg arg param =
         if not (is_base decls arg param)
         then err (Printf.sprintf "invalid call argument: type '%s' is incompatible with '%s'" arg param)

    in if List.length args <> List.length decl.ctorParams
    then err (Printf.sprintf "invalid number of arguments in instantiation of '%s'" className);

    List.iter2 (fun arg (param: ctorParam) ->
        check_arg arg param.className
      ) args decl.ctorParams
and check_expr_cast decls env (className, e) = 
  check_expr decls env e;
  
  let decl = get_class_opt decls className 
  in match decl with 
  | None -> err (Printf.sprintf "cast to unkown class '%s' " className)
  | Some(_decl) -> 
    let t = get_expr_type decls env e
    in if not (is_base decls t className) 
       then err(Printf.sprintf "cannot cast '%s' to '%s' " t className)   

and check_expr_op decls env (e1, e2) =
  check_expr decls env e1;
  check_expr decls env e2;
  let t1 = get_expr_type decls env e1
  in let t2 = get_expr_type decls env e2
  in if t1 <> "Integer" ||  t2 <> "Integer"
  then err (Printf.sprintf "numeric infix operators expect Integer expressions on both side, got '%s' op '%s'" t1 t2);

and check_expr_strcat decls env (e1, e2) =
  check_expr decls env e1;
  check_expr decls env e2;
  let t1 = get_expr_type decls env e1
  in let t2 = get_expr_type decls env e2
  in if t1 <> "String" ||  t2 <> "String"
  then err (Printf.sprintf "string concatenation operator expects String expressions, got '%s' ^ '%s'" t1 t2);

  (** Checks an expression.
      @raise Contextual_error if a check fails. *)

and check_expr decls env expr =
  match expr with
  | Id id -> check_in_scope env id
  | Attr(e, name) -> check_expr_attr decls env (e, name)
  | StaticAttr(className, name) -> check_expr_static_attr decls (className, name)
  | UMinus e -> check_expr decls env e
  | List le -> List.iter (check_expr decls env) le
  | Call(e, methName, args) -> check_expr_call decls env (e, methName, args)
  | StaticCall(className, methName, args) -> check_expr_static_call decls env (className, methName, args)
  | BinOp(e1, _, e2) -> check_expr_op decls env (e1, e2)
  | StrCat(e1, e2) -> check_expr_strcat decls env (e1, e2)
  | New(className, args) -> check_expr_new decls env (className, args)
  | StaticCast (className, e) -> check_expr_cast decls env (className, e)
  | Cste _ | String _ -> ()

(* -------------------------------------------------------------------------- *)

let check_static_method decls env meth =
  check_no_reserved_var meth.params;
  let env = make_method_env env meth
  in check_instr decls env meth.body;
  match meth.retType with
  | Some _ -> check_returns meth.body
  | None -> ()

let check_instance_method decls env meth =
  check_no_reserved_var meth.params;
  let env = make_method_env env meth
  in check_instr decls env meth.body;
  match meth.retType with
  | Some _ -> check_returns meth.body
  | None -> ()

let check_main_instr decls instr =
  check_instr decls [] instr

let check_decl decls decl =
  check_no_reserved_var decl.body.instAttrs;
  check_no_reserved_var decl.body.staticAttrs;
  check_ctor decl;
  check_overrides decls decl;
  check_no_dup decl;
  let env = make_class_env decl
  in List.iter (check_instance_method decls env) decl.body.instMethods;
  List.iter (check_static_method decls []) decl.body.staticMethods

let check_decls decls =
  check_no_reserved_class decls;
  check_no_dup_class decls;
  check_inheritance decls;
  check_cycles decls;
  List.iter (check_decl decls) decls

(** Perform all checks on ast.
    @raise Contextual_error if a check fails. *)

let check_all ast =
  check_decls ast.decls;
  check_main_instr ast.decls ast.instr

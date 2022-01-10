open Ast

(** Get the class declaration with a given name. *)

let get_class_opt decls name =
  List.find_opt (fun decl -> decl.name = name) decls

(** Get the class declaration with a given name.
    @raise Not_found if no such declaration is found. *)

let get_class decls name =
  get_class_opt decls name
  |> Optmanip.get_or_else (fun () ->
      Printf.eprintf "[ERR] get_class '%s' failed\n" name;
      raise Not_found
    )

(** Utility to run function recursively through superclasses until it returns Some. *)

let rec recursively decls decl f =
  f decl
  |> Optmanip.or_else (fun () ->
      match decl.superclass with
      | None -> None
      | Some(super) ->
        (get_class_opt decls super)
        |> Optmanip.and_then (fun superDecl ->
            recursively decls superDecl f
          )
    )

(** List of all ancestor class declarations in bottom-to-top order.
    @raise Not_found if an ancestor has no declaration. *)

let rec ancestors decls decl =
  match decl.superclass with
  | None -> []
  | Some(super) ->
    let superDecl = (get_class decls super) in
    superDecl :: (ancestors decls superDecl)

(** Get the method declaration in a class with a given name. *)

let get_method_opt name decl =
  List.find_opt (fun (meth: methodDecl) -> meth.name = name) decl.body.instMethods

(** Find (recursively through ancestors) the method declaration in a class
    with a given name. *)

let find_method_opt decls name decl =
  (get_method_opt name) |> recursively decls decl

(** Find (recursively through ancestors) the method declaration in a class
    with a given name.
    @raise Not_found if the class has no such static method. *)

let find_method decls name decl =
  find_method_opt decls name decl
  |> Optmanip.get_or_else (fun () ->
     Printf.eprintf "[ERR] get_static_method_opt '%s' failed\n" name; 
      raise Not_found
    )

(** Get a static method declaration in a class with a given name. *)

let get_static_method_opt name decl =
  List.find_opt (fun (meth: methodDecl) -> meth.name = name) decl.body.staticMethods

(** Get a static method declaration in a class with a given name.
    @raise Not_found if the class has no such static method. *)

let get_static_method name decl =
  get_static_method_opt name decl
  |> Optmanip.get_or_else (fun () ->
     Printf.eprintf "[ERR] get_static_method_opt '%s' failed\n" name; 
      raise Not_found
    )

(** Get the type of an attribute in a class declaration. *)

let get_inst_attr_opt attrName decl =
  let pred (attr: param) =
    if attr.name = attrName then Some(attr.className) else None
  in let pred2 (attr: ctorParam) =
       if attr.name = attrName then Some(attr.className) else None
  in List.find_map pred decl.body.instAttrs
     |> Optmanip.or_else (fun () ->
         List.find_map pred2 decl.ctorParams
       )

(** Find (recursively through ancestors) the type of an attribute in a class declaration. *)

let find_inst_attr_opt decls attrName decl =
  (get_inst_attr_opt attrName) |> recursively decls decl

(** Find (recursively through ancestors) the type of an attribute in a class declaration. 
    @raise Not_found if the class has no such attribute. *)

let find_inst_attr decls attrName decl =
  find_inst_attr_opt decls attrName decl
  |> Optmanip.get_or_else (fun () ->
      Printf.eprintf "[ERR] find_inst_attr '%s' failed\n" attrName;
      raise Not_found
    )

(** Get the type of a static attribute in a class declaration. *)

let get_static_attr_opt attrName decl =
  let pred (attr: param) =
    if attr.name = attrName then Some(attr.className) else None
  in List.find_map pred decl.body.staticAttrs

(** Get the type of a static attribute in a class declaration.
    @raise Not_found if the class has no such attribute. *)

let get_static_attr attrName decl =
  get_static_attr_opt attrName decl
  |> Optmanip.get_or_else (fun () ->
      Printf.eprintf "[ERR] get_static_attr '%s' failed\n" attrName;
      raise Not_found
    )

(** Get the type of a method in a class declaration.
    Note: procedures have the special type '_Void' *)

let get_inst_method_type_opt methName decl =
  decl.body.instMethods
  |> List.find_map (fun (meth: methodDecl) ->
      if meth.name = methName then meth.retType |> Optmanip.or_ (Some("_Void")) else None
    )

(** Find (recursively through ancestors) the type of a method in a class declaration.
    Note: procedures have the special type '_Void' *)

let find_inst_method_type_opt decls attrName decl =
  (get_inst_method_type_opt attrName) |> recursively decls decl

(** Find (recursively through ancestors) the type of a method in a class declaration.
    Note: procedures have the special type '_Void'
    @raise Not_found if the class has no such attribute. *)

let find_inst_method_type decls attrName decl =
  find_inst_method_type_opt decls attrName decl
  |> Optmanip.get_or_else (fun () ->
      Printf.eprintf "[ERR] get_static_attr '%s' failed\n" attrName;
      raise Not_found
    )

(** Get the type of a static method in a class declaration.
    Note: procedures have the special type '_Void' *)

let get_static_method_type methName decl =
  decl.body.staticMethods
  |> List.find_map  (fun (meth: methodDecl) ->
      if meth.name = methName then meth.retType else None
    )
  |> Optmanip.get_or("_Void")

(** Computes an expression type. *)

let get_expr_type decls env expr =
  let rec r_get expr =
    match expr with
    | Cste _ | BinOp _ | UMinus _ -> "Integer"
    | String _ | StrCat _ -> "String"

    | Id id -> Util.Env.get env id

    | Attr(e, attrName) ->
      let decl = get_class decls (r_get e)
      in find_inst_attr decls attrName decl

    | StaticAttr(className, attrName) ->
      let decl = get_class decls className
      in get_static_attr attrName decl

    | List l ->
      let last = List.hd (List.rev l)
      in r_get last

    | Call(caller, name, _args) ->
      let t = r_get caller
      in if t = "String" then "_Void" (* String methods return _Void (print and println) *)
      else if t = "Integer" then "String" (* String methods return String (toString) *)
      else
        let decl = get_class decls (r_get caller)
        in find_inst_method_type decls name decl

    | StaticCall(className, name, _args) ->
      let decl = get_class decls className
      in get_static_method_type name decl

    | New(className, _args) -> className
    | StaticCast(className, _args) -> className 

  in r_get expr

(** Wether derived is convertible to base. *)

let is_base decls derived base =
  if derived = base then true
  else
    let derived = get_class decls derived
    in let base = get_class decls base
    in List.exists ((=) base) (ancestors decls derived)


(** Make an environment with 'super' and 'this'. *)

let make_class_env decl =
  let env = ("this", decl.name) :: []
  in let env = match decl.superclass with
      | Some(super) -> ("super", super) :: env
      | None -> env
  in env

(** Make an environment with method params and optionally 'result'. *)

let make_method_env env meth =
  let env = Util.Env.add_all env meth.params
  in let env = match meth.retType with
      | Some(ret) -> ("result", ret) :: env
      | None -> env
  in env

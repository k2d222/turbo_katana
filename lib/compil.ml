open Ast
open Astmanip

let lbl_counter = ref 0

(** Generate a unique label. *)
  
let genlbl () =
  lbl_counter := !lbl_counter + 1;
  "lbl" ^ string_of_int !lbl_counter

(** Generate a unique method label. *)

let meth_lbl className methName =
  Printf.sprintf "%s_%i_%s" className (String.length methName) methName

(** Generate a unique constructor label. *)

let ctor_lbl className =
  Printf.sprintf "_CTOR_%s_" className
  
(** Get the address of a local variable. *)

let get_addr addrs id =
  match List.assoc_opt id addrs with
  | Some(addr) -> addr
  | None -> failwith (Printf.sprintf "variable %s not found in addrs" id)

(** Make a lookup between method parameter names and their address *)

let make_method_addrs params =
    let len = List.length params
    in let addrs = params |> List.mapi (fun i (p: param) -> 
        (p.name, -len + i)
      )
    in let addrs = ("result", -len - 2)::addrs
    in let addrs = ("this", -len - 1)::addrs
    in addrs

(** Get a list of all instance attributes in a class, in offset order. *)

let rec all_attrs decls decl = 
  let attrs = 
    List.map (fun ({ name; _ }: param) -> name) decl.instAttrs
  in match decl.superclass with
  | None -> attrs
  | Some(super) ->
    let super = get_class decls super
    in all_attrs decls super @ attrs
  
(** Get the offset of an instance attribute in a class. *)

let attr_offset decls decl attrName =
  Util.index_of (all_attrs decls decl) attrName
    
(* --------------------------------------------- *)

(** Put the code of the program on the output channel. *)

let compile chan ast =
  let decls = ast.decls in

  let _NOP () = Printf.fprintf chan "NOP\n" in
  let _ERR = Printf.fprintf chan "ERR %s\n" in
  let _START () = Printf.fprintf chan "START\n" in
  let _STOP () = Printf.fprintf chan "STOP\n" in
  let _PUSHI = Printf.fprintf chan "PUSHI %i\n" in
  let _PUSHS = Printf.fprintf chan "PUSHS %s\n" in
  let _PUSHG = Printf.fprintf chan "PUSHG %i\n" in
  let _PUSHL = Printf.fprintf chan "PUSHL %i\n" in
  let _PUSHSP = Printf.fprintf chan "PUSHSP %i\n" in
  let _PUSHFP = Printf.fprintf chan "PUSHFP %i\n" in
  let _STOREL = Printf.fprintf chan "STOREL %i\n" in
  let _STOREG = Printf.fprintf chan "STOREG %i\n" in
  let _PUSHN = Printf.fprintf chan "PUSHN %i\n" in
  let _POPN = Printf.fprintf chan "POPN %i\n" in
  let _DUPN = Printf.fprintf chan "DUPN %i\n" in
  let _SWAP () = Printf.fprintf chan "SWAP\n" in
  let _EQUAL () = Printf.fprintf chan "EQUAL\n" in
  let _NOT () = Printf.fprintf chan "NOT\n" in
  let _JUMP = Printf.fprintf chan "JUMP %s\n" in
  let _JZ = Printf.fprintf chan "JZ %s\n" in
  let _PUSHA = Printf.fprintf chan "PUSHA %s\n" in
  let _CALL () = Printf.fprintf chan "CALL\n" in
  let _RETURN () = Printf.fprintf chan "RETURN\n" in
  let _ADD () = Printf.fprintf chan "ADD\n" in
  let _SUB () = Printf.fprintf chan "SUB\n" in
  let _MUL () = Printf.fprintf chan "MUL\n" in
  let _DIV () = Printf.fprintf chan "DIV\n" in
  let _INF () = Printf.fprintf chan "INF\n" in
  let _INFEQ () = Printf.fprintf chan "INFEQ\n" in
  let _SUP () = Printf.fprintf chan "SUP\n" in
  let _SUPEQ () = Printf.fprintf chan "SUPEQ\n" in
  let _WRITEI () = Printf.fprintf chan "WRITEI\n" in
  let _STR () = Printf.fprintf chan "STR\n" in
  let _WRITES () = Printf.fprintf chan "WRITES\n" in
  let _CONCAT () = Printf.fprintf chan "CONCAT\n" in
  let _STORE = Printf.fprintf chan "STORE %d\n" in
  let _LOAD = Printf.fprintf chan "LOAD %d\n" in
  let _ALLOC = Printf.fprintf chan "ALLOC %d\n" in
  let _LABEL = Printf.fprintf chan "%s: NOP\n" in
  let _COMMENT = Printf.fprintf chan "-- %s\n" in
  
  let code_op op = match op with
    | Eq -> _EQUAL ()
    | Neq -> _EQUAL (); _NOT ()
    | Lt -> _INF ()
    | Le -> _INFEQ ()
    | Gt -> _SUP ()
    | Ge -> _SUPEQ ()
    | Add -> _ADD ()
    | Sub -> _SUB ()
    | Mul -> _MUL ()
    | Div -> _DIV () in
  
  let rec code_instr_ite attrs env (cmp, yes, no) =
    let lbl_else = genlbl () in
    let lbl_end = genlbl () in
    code_expr attrs env cmp;
    _JZ lbl_else;
    (code_instr attrs env yes);
    _JUMP lbl_end;
    _LABEL lbl_else;
    (code_instr attrs env no);
    _LABEL lbl_end;
  
  and code_instr_block attrs env (lp, li) =
    ()
 
  and code_instr_assign attrs env (from_, to_) =
    ()
    
  (** Code to compute an instruction.
      Leave nothing on the stack after execution. *)
  
  and code_instr addrs env instr = 
    match instr with
    | Block(lp, li) -> code_instr_block addrs env (lp, li)
    | Assign(from_, to_) -> code_instr_assign addrs env (from_, to_)
    | Return -> _RETURN ()
    | Ite(cmp, yes, no) -> code_instr_ite addrs env (cmp, yes, no)
    | Expr(e) -> code_expr addrs env e; _POPN 1 
  
  and code_expr_attr addrs env (e, s) = 
    code_expr addrs env e;
    let decl = get_expr_type decls env e 
    in let decl = get_class decls decl
    in _LOAD ((attr_offset decls decl s) + 1)
  
  and code_builtin_string addrs env e m = 
    code_expr addrs env e; (* push this *)

    match m with
    | "print" -> _DUPN 1; _WRITES ()
    | "println" -> _DUPN 1; _WRITES (); _PUSHS "\n"; _WRITES ()
    | _ -> failwith "unreachable"
    
  and code_builtin_integer addrs env e m =
    code_expr addrs env e; (* push this *)

    match m with
    | "toString" -> _STR ()
    | _ -> failwith "unreachable"
  
  and code_expr_call addrs env (e, s, le) = 
    let clName = get_expr_type decls env e
      
    in match clName with
    | "Integer" -> code_builtin_integer addrs env e s
    | "String" -> code_builtin_string addrs env e s
    | _ ->
      let decl = get_class decls clName
      in let vt = Vtable.make decls decl
      in let meth = find_method decls s decl
      in _PUSHI 0; (* push result *)
      List.iter (code_expr addrs env) le; (* push args *)
      code_expr addrs env e; (* push this *)
      _DUPN 1;
      _LOAD 0;
      _LOAD (Vtable.offset vt meth);
      _CALL ();
      _POPN ((List.length le) + 1) (* pop args & this, leave result *)
  
  and code_expr_static_attr addrs env (clName, attrName) =
    ()
  
  and code_expr_static_call (clName, methName, args) =
    ()
  
  and code_expr_new (clName, args) =
    ()
        
  (** Code to compute an expression.
      Leave a pointer to the expr result after execution. *)
  
  and code_expr addrs env e =
    match e with
      | Id(id) -> _PUSHL (get_addr addrs id)
      | Cste(c) -> _PUSHI c
      | Attr(e, s) -> code_expr_attr addrs env (e, s)
      | StaticAttr(clName, attrName) -> code_expr_static_attr addrs env (clName, attrName)
      | UMinus(e) -> _PUSHI 0; code_expr addrs env e; _SUB ()
      | Call(e, s, le) -> code_expr_call addrs env (e, s, le) 
      | StaticCall(clName, methName, args) -> code_expr_static_call (clName, methName, args)
      | BinOp(e1, op, e2) -> code_expr addrs env e1; code_expr addrs env e2; code_op op
      | String(s) -> _PUSHS s
      | StrCat(s1, s2) -> code_expr addrs env s1; code_expr addrs env s2; _CONCAT ()
      | New(clName, args) -> code_expr_new (clName, args)
      | StaticCast(_, e) -> code_expr addrs env e
  
  (** Generate a virtual table for a class.
      Leave a pointer to the VT on stack after execution. *)
  
  in let code_vtable decl =
    let vt = Vtable.make decls decl
    in if vt <> [] then 
      _ALLOC (List.length vt);
      vt |> List.iteri (fun i (name, decl) -> 
        _DUPN 1;
        _PUSHA (meth_lbl decl.name name);
        _STORE i
      )
  
  (** Code of a call to super constructor.
      Expects 'this' on stack and leave 'this' after execution. *)
  
  in let code_super_call addrs env decl =
    let args = snd (decl.ctor.superCall |> Option.get)
    in let super = decl.superclass |> Option.get
    in List.iter (code_expr addrs env) args; (* push args *)
    _PUSHL (-1 - List.length args); (* push this *)
    _PUSHA (ctor_lbl super);
    _CALL ();
    _POPN ((List.length args) + 1) (* pop args & this *)

  (** Code of a constructor. *)
  
  in let code_ctor decl =
    let size = List.length (all_attrs decls decl) + 1
    in let params = ctor_params_to_method_params decl.ctor.params
    in let addrs = make_method_addrs params
    in let env = Env.add_all [] params
    in let vti = Util.index_of decls decl

    in let rec call_super_ctor decl = 
      match decl.superclass with 
      | Some(_) -> code_super_call addrs [] decl        
      | None -> ()

    in _LABEL (ctor_lbl decl.name);
    _ALLOC size;
    _PUSHI vti;
    _STORE 0;
    call_super_ctor decl;
    code_instr addrs env decl.ctor.body

  in let code_inst_method meth =
    ()
  
  in let code_static_method meth =
    ()
 
  in let code_main_instr instr =
    ()
  
  in let code_static_attr attr =
    ()

in
  _COMMENT "----- VTABLES -----";
  List.iter code_vtable decls;

  _COMMENT "----- STATIC ATTRIBS -----";
  List.iter (fun decl -> code_static_attr decl.staticAttrs) decls;

  _COMMENT "----- MAIN INSTRUCTION -----";
  code_main_instr ast.instr;

  _COMMENT "----- FUNCTIONS -----";
  decls |> List.iter (fun decl ->
      code_ctor decl;
      decl.instMethods |> List.iter code_inst_method;
      decl.staticMethods |> List.iter code_static_method
    );

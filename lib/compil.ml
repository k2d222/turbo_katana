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

let static_lbl className methName =
	Printf.sprintf "%s_%i_%s" className (String.length methName) methName

  
(** Get the address of a local variable. *)

let get_addr addrs id =
  match List.assoc_opt id addrs with
  | Some(addr) -> addr
  | None -> failwith (Printf.sprintf "variable %s not found in addrs" id)

(** Make a lookup between method parameter names and their address *)

let addrs_add addrs name =
	let prev = List.fold_left (fun acc (_name, addr) -> max acc addr) (-1) addrs
	in (name, prev + 1)::addrs 

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
  in match decl.super with
  | None -> attrs
  | Some(super) ->
    let super = get_class decls super.name
    in all_attrs decls super @ attrs
  
(** Get the offset of an instance attribute in a class. *)

let attr_offset decls decl attrName =
  all_attrs decls decl |> Util.index_of attrName 

let static_attr_offset decls decl attr =  
	let rec marcel decls =
		match decls with 
		| d::_ when decl = d -> 
			List.map (fun (a: param) -> a.name) d.staticAttrs 
			|> Util.index_of attr
		| d::r -> (List.length d.staticAttrs) + marcel r 
		| _ -> failwith "static_attr_offset unreachable"
	in marcel decls
    
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
  
  let rec code_instr_ite addrs env (cmp, yes, no) =
    let lbl_else = genlbl () in
    let lbl_end = genlbl () in
    code_expr addrs env cmp;
    _JZ lbl_else;
    (code_instr addrs env yes);
    _JUMP lbl_end;
    _LABEL lbl_else;
    (code_instr addrs env no);
    _LABEL lbl_end;
  
  and code_instr_block addrs env (lp, li) =
    _PUSHN (List.length lp);
		let addrs = List.fold_left (fun addrs (p: param) -> addrs_add addrs p.name) addrs lp
		in let env = List.fold_left (fun env (p: param) -> Env.add env p) env lp
		in List.iter (code_instr addrs env) li

  and code_instr_assign addrs env (to_, from_) =
		match to_ with
		| Attr(e, s) -> 
			let name = get_expr_type decls env e 
			in let decl = get_class decls name
			in let off = attr_offset decls decl s
			in code_expr addrs env to_; 
			code_expr addrs env from_;
			_STORE off

		| StaticAttr(name, s) -> 
			let decl = get_class decls name
			in let off = static_attr_offset decls decl s
			in code_expr addrs env to_; 
			code_expr addrs env from_;
			_STORE off
			

		| Id(s) -> 
			let addr = get_addr addrs s
			in code_expr addrs env to_; 
			code_expr addrs env from_;
			_STOREL addr

		| _ -> failwith "code_instr_assign unreachable"
    
  (** Code to compute an instruction.
      Leave nothing on the stack after execution. *)
  
  and code_instr addrs env instr = 
    match instr with
    | Block(lp, li) -> code_instr_block addrs env (lp, li)
    | Assign(to_, from_) -> code_instr_assign addrs env (to_, from_)
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
    | _ -> failwith "code_builtin_string unreachable"
    
  and code_builtin_integer addrs env e m =
    code_expr addrs env e; (* push this *)

    match m with
    | "toString" -> _STR ()
    | _ -> failwith "code_builtin_integer unreachable"
  
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
  
  and code_expr_static_call addrs env (clName, methName, args) =
		let name = static_lbl clName methName
		in List.iter (code_expr addrs env) args;
		_PUSHA name;
		_CALL ()

  and code_expr_new (clName, args) =
		let name = ctor_lbl clName
		in _PUSHA name;
		_CALL ()
        
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
      | StaticCall(clName, methName, args) -> code_expr_static_call addrs env (clName, methName, args)
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
    let { args; name } = Option.get decl.super
    in List.iter (code_expr addrs env) args; (* push args *)
    _PUSHL (-1 - List.length args); (* push this *)
    _PUSHA (ctor_lbl name);
    _CALL ();
    _POPN ((List.length args) + 1) (* pop args & this *)

  (** Code of a constructor. *)
  
  in let code_ctor decl =
    let size = List.length (all_attrs decls decl) + 1
    in let params = ctor_params_to_method_params decl.ctor.params
    in let addrs = make_method_addrs params
    in let env = make_class_env decl
    in let env = Env.add_all env params
    in let vti = Util.index_of decl decls

    in let rec call_super_ctor decl = 
      match decl.super with 
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
  
  in let code_static_attrs () =
    let size = List.fold_left (fun acc decl -> acc + (List.length decl.staticAttrs)) 0 decls
		in _PUSHN size

in
  _COMMENT "----- VTABLES -----";
  List.iter code_vtable decls;

  _COMMENT "----- STATIC ATTRIBS -----";
	code_static_attrs ();

  _COMMENT "----- MAIN INSTRUCTION -----";
  code_main_instr ast.instr;

  _COMMENT "----- FUNCTIONS -----";
  decls |> List.iter (fun decl ->
      code_ctor decl;
      decl.instMethods |> List.iter code_inst_method;
      decl.staticMethods |> List.iter code_static_method
    );

(*	><,`C   --------- ><> *)  
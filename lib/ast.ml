type opComp = Eq | Neq | Lt | Le | Gt | Ge
[@@deriving show]

type instr =
  | Block of param list * instr list
  | Assign of expr * expr
  | Return
  | Ite of expr * instr * instr
  | Expr of expr

and expr =
  | Id of string
  | Cste of int
  | Select of expr * string
  | Plus of expr * expr
  | Minus of expr * expr
  | UMinus of expr
  | Times of expr * expr
  | Div of expr * expr
  | List of expr list
  | MethodCall of expr * expr list
  | AttrOf of expr * expr
  | Comp of expr * opComp * expr
  | String of string 
  | StrCat of expr * expr
  | New of string * expr list

and param = {
  name: string;
  className: string;
}
[@@deriving show]

type methodDecl = {
  name: string;
  params: param list;
  retType: string option;
  body: instr;
}
[@@deriving show]

type classBody = {
  methods: methodDecl list;
  staticAttrs: param list;
  instAttrs: param list;
}
[@@deriving show]

type classDecl = {
  name: string;
  ctorParams: param list;
  body: classBody;
  superclass: string option;
}
[@@deriving show]

type prog = {
  decls: classDecl list;
  instr: instr;
}
[@@deriving show]

exception VC_error of string (* erreur contextuelle *)
exception Run_error of string (* erreur a l'execution pour un interprete *)
exception Internal_error of string

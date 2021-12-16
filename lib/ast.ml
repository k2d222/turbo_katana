type opComp = Eq | Neq | Lt | Le | Gt | Ge
[@@deriving show]

type instr =
  | Block of param list * instr list
  | Assign of expr * expr
  | Return of expr
  | Ite of expr * instr * instr
  | Expr of expr

and expr =
  | Id of string
  | Cste of int
  | AttrOf of expr * string
  | Plus of expr * expr
  | Minus of expr * expr
  | UMinus of expr
  | Times of expr * expr
  | Div of expr * expr
  | List of expr list
  | MethodCall of string * expr * expr list
  | Comp of expr * opComp * expr
  | String of string
  | StrCat of expr * expr
  | New of string * expr list

and param = {
  name: string;
  className: string;
}
[@@deriving show]

type ctorDecl = {
  name: string;
  params: param list;
  superCall: (string * expr list) option;
  body: instr;
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
  ctor: ctorDecl;
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

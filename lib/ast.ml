type numBinOp =
  | Eq | Neq
  | Lt | Le | Gt | Ge
  | Add | Sub | Mul | Div
[@@deriving show]

type param = {
  name: string;
  className: string;
}
[@@deriving show]

type ctorParam = {
  isMember: bool;
  name: string;
  className: string;
}
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
  | Attr of expr * string
  | StaticAttr of string * string
  | UMinus of expr
  | List of expr list
  | Call of expr * string * expr list
  | StaticCall of string * string * expr list
  | BinOp of expr * numBinOp * expr
  | String of string
  | StrCat of expr * expr
  | New of string * expr list
  | StaticCast of string * expr
[@@deriving show]

type ctorDecl = {
  name: string;
  params: ctorParam list;
  superCall: (string * expr list) option;
  body: instr;
}
[@@deriving show]

type methodDecl = {
  name: string;
  override: bool;
  params: param list;
  retType: string option;
  body: instr;
}
[@@deriving show]

type classBody = {
  ctor: ctorDecl;
  staticMethods: methodDecl list;
  instMethods: methodDecl list;
  staticAttrs: param list;
  instAttrs: param list;
}
[@@deriving show]

type classDecl = {
  name: string;
  ctorParams: ctorParam list;
  body: classBody;
  superclass: string option;
}
[@@deriving show]

type prog = {
  decls: classDecl list;
  instr: instr;
}
[@@deriving show]

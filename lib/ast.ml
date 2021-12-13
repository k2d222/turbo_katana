type opComp = Eq | Neq | Lt | Le | Gt | Ge
[@@deriving show]

type expType =
  | Id of string
  | Cste of int
  | Plus of expType*expType
  | Minus of expType*expType
  | UMinus of expType
  | Times of expType*expType
  | Div of expType*expType
  | Ite of compType*expType*expType
[@@deriving show]

and
compType = Comp of opComp*expType*expType
[@@deriving show]

type declType = {
  lhs: string;
  rhs: expType;
}
[@@deriving show]

type progType = {
  decls: declType list;
  expr: expType;
}
[@@deriving show]

exception VC_error of string (* erreur contextuelle *)
exception Run_error of string (* erreur a l'execution pour un interprete *)
exception Internal_error of string

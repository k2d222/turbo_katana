(* open Ast

let printExpr e =
  Printf.printf "%s\n" (Misc.string_of_expr e)

let printDecl d =
  Printf.printf "%s := %s;\n" d.lhs (Misc.string_of_expr d.rhs)

let print prog =
  List.iter printDecl prog.decls;
  Printf.printf "\nbegin\n  %s\nend\n" (Misc.string_of_expr prog.expr) *)

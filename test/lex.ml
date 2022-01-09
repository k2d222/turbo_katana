open Utils

(* ---------- Lexical tests ---------- *)

let%test "invalid-token" =
  expects_err {| # |} && 
  expects_err {| ~ |} && 
  expects_err {| ! |} && 
  expects_err {| @ |} && 
  expects_err {| % |} && 
  expects_err {| | |} && 
  expects_err {| ' |} 

let%test "valid-token" =
  expects_parse_err {| x |} &&
  expects_parse_err {| X |} &&
  expects_parse_err {| 42 |} &&

  expects_parse_err {| class |} &&
  expects_parse_err {| extends |} &&
  expects_parse_err {| new |} &&
  expects_parse_err {| is |} &&
  expects_parse_err {| var |} &&
  expects_parse_err {| def |} &&
  expects_parse_err {| static |} &&
  expects_parse_err {| override |} &&
  expects_parse_err {| return |} &&
  expects_parse_err {| if |} &&
  expects_parse_err {| then |} &&
  expects_parse_err {| else |} &&

  expects_parse_err {| xclass |} &&
  expects_parse_err {| Class |} &&

  expects_parse_err {| + |} &&
  expects_parse_err {| - |} &&
  expects_parse_err {| * |} &&
  expects_parse_err {| / |} &&
  expects_parse_err {| & |} &&
  (* expects_parse_err {| " |} && *)
  expects_parse_err {| > |} &&
  expects_parse_err {| < |} &&
  expects_parse_err {| >= |} &&
  expects_parse_err {| <= |} &&
  expects_parse_err {| = |} &&
  expects_parse_err {| <> |} &&
  expects_parse_err {| := |} &&
  expects_parse_err {| ; |} &&
  expects_parse_err {| : |} &&
  expects_parse_err {| , |} &&
  expects_parse_err {| . |} &&
  expects_parse_err {| ( |} &&
  expects_parse_err {| ) |} &&
  expects_parse_err {| { |} &&
  expects_parse_err {| } |} 


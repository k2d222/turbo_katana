open Utils

(* ---------- Parsing tests ---------- *)

let%test "empty-prog" =
  expects_parse_err {| |}

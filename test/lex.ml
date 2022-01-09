open Utils

(* ---------- Lexical tests ---------- *)

let%test "invalid-token" =
  expects_err {| # |}



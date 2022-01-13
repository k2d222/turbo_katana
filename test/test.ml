open Utils

let%test_unit "ex1-parse" =
    ignore @@ parse_file "../progs/ex1.kat"
    
let%test_unit "ex1-V3-file-parse" = 
    ignore @@ parse_file "../progs/ex1-V3.kat"

let%test_unit "gn-file-parse" = 
    ignore @@ parse_file "../progs/gn.kat"

let%test_unit "grandMere-file-parse" = 
    ignore @@ parse_file "../progs/grandMere.kat"

let%test_unit "masking1-file-parse" = 
    ignore @@ parse_file "../progs/masking1.kat"

let%test_unit "test-file-parse" = 
    ignore @@ parse_file "../progs/test.kat"


let%test "err1-file-parse" = 
    file_ctx_err "../progs/err1.kat"

let%test "err2-file-parse" = 
    file_ctx_err "../progs/err2.kat"

let%test "err3-file-parse" = 
    file_ctx_err "../progs/err3.kat"
    
let%test "err4-file-parse" = 
    file_ctx_err "../progs/err4.kat"

let%test "errCast-file-parse" = 
    file_ctx_err "../progs/errCast.kat"

let%test "errCycle-file-parse" = 
    file_ctx_err "../progs/errCycle.kat"

let%test "errResult-file-parse" = 
    file_ctx_err "../progs/errResult.kat"

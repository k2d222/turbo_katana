open Utils

let%test "ex1-parse" =
    file_ast "../progs/ex1.kat"
    
let%test "gn-file-parse" = 
    file_ast "../progs/gn.kat"

let%test "grandMere-file-parse" = 
    file_ast "../progs/grandMere.kat"

let%test "masking1-file-parse" = 
    file_ast "../progs/masking1.kat"

let%test "test-file-parse" = 
    file_ast "../progs/test.kat"

let%test "list-file-parse" = 
    file_ast "../progs/list.kat"

let%test "math-file-parse" = 
    file_ast "../progs/math.kat"


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

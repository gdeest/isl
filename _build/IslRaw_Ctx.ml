open Ctypes
open Foreign
open IslRaw_Types

(* isl_ctx *isl_ctx_alloc(); *)
let alloc = foreign "isl_ctx_alloc" (void @-> returning ctx)
(* void isl_ctx_free(isl_ctx *ctx); *)    
let free = foreign "isl_ctx_free" (ctx @-> returning void)

(* void isl_ctx_set_max_operations(isl_ctx *ctx, unsigned long max_operations); *)
let set_max_operations = foreign "isl_ctx_set_max_operations" (ctx @-> ulong @-> returning void)
(* unsigned long isl_ctx_get_max_operations(isl_ctx *ctx); *)
let get_max_operations = foreign "isl_ctx_get_max_operations" (ctx @-> returning ulong)
(* void isl_ctx_reset_operations(isl_ctx *ctx); *)
let reset_operations = foreign "isl_ctx_reset_operations" (ctx @-> returning void)

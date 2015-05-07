open Ctypes
open Foreign
open IslRaw_Types

(* FIXME: Any way to attach user-data to IDs in a mostly typesafe way ? *)
(* __isl_give isl_id *isl_id_alloc(isl_ctx *ctx, __isl_keep const char *name, void *user); *)    
let alloc = foreign "isl_id_alloc" (ctx @-> string @-> ptr void @-> returning id)
    
(* __isl_give isl_id *isl_id_set_free_user(__isl_take isl_id *id, __isl_give void (\*free_user)(void *user)); *)
(* TODO *)
    
(* __isl_give isl_id *isl_id_copy(isl_id *id); *)    
let copy = foreign "isl_id_copy" (id @-> returning id)
(* __isl_null isl_id *isl_id_free(__isl_take isl_id *id); *)    
let free = foreign "isl_id_free" (id @-> returning void)

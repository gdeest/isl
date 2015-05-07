open Ctypes
open Foreign
open IslRaw_Types

(* __isl_give isl_aff *isl_aff_copy(__isl_keep isl_aff *aff); *)
let copy = foreign "isl_aff_copy" (aff @-> returning aff)
(* __isl_null isl_aff *isl_aff_free(__isl_take isl_aff *aff); *)
let free = foreign "isl_aff_free" (aff @-> returning void)


open Ctypes
open Foreign
open IslRaw_Types

(* isl_give isl_local_space *isl_local_space_copy(__isl_keep isl_local_space *ls); *)
let copy = foreign "isl_local_space_copy" (local_space @-> returning local_space)
(* __isl_null isl_local_space *isl_local_space_free(__isl_take isl_local_space *ls); *)
let free = foreign "isl_local_space_free" (local_space @-> returning void)
(* __isl_give isl_local_space *isl_local_space_from_space(__isl_take isl_space *space); *)
let from_space = foreign "isl_local_space_from_space" (space @-> returning local_space)

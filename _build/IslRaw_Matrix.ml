open Ctypes
open Foreign
open IslRaw_Types

let copy = foreign "isl_mat_copy" (matrix @-> returning matrix)
let free = foreign "isl_mat_free" (matrix @-> returning void)    

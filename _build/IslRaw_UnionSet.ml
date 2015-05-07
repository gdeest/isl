open Ctypes
open Foreign
open IslRaw_Types

let copy = foreign "isl_union_set_copy" (union_set @-> returning union_set)
let free = foreign "isl_union_set_free" (union_set @-> returning void)
let empty = foreign "isl_union_set_empty" (space @-> returning union_set)
let universe = foreign "isl_union_set_universe" (space @-> returning union_set)
let from_set = foreign "isl_union_set_from_set" (set @-> returning union_set)    
let isl_set_from_union_set = foreign "isl_set_from_union_set" (union_set @-> returning set)

open Ctypes
open Foreign
open IslRaw_Types
    
let copy = foreign "isl_union_map_copy" (union_map @-> returning union_map)
let free = foreign "isl_union_map_free" (union_map @-> returning void)
let empty = foreign "isl_union_map_empty" (space @-> returning union_map)
let universe = foreign "isl_union_map_universe" (space @-> returning union_map)
let from_map = foreign "isl_union_map_from_map" (map @-> returning union_map)
let from_union_map = foreign "isl_map_from_union_map" (union_map @-> returning map)

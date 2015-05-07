open Ctypes
open Foreign
open IslRaw_Types

let copy = foreign "isl_basic_map_copy" (basic_map @-> returning basic_map)
let free = foreign "isl_basic_map_free" (basic_map @-> returning void)
let empty = foreign "isl_basic_map_empty" (space @-> returning basic_map)
let universe = foreign "isl_basic_map_universe" (space @-> returning basic_map)
let identity = foreign "isl_basic_map_identity" (space @-> returning basic_map)
let nat_universe = foreign "isl_basic_map_nat_universe" (space @-> returning basic_map)
let add_constraint = foreign "isl_basic_map_add_constraint" (basic_map @-> constrnt @-> returning basic_map)
let get_local_space = foreign "isl_basic_map_get_local_space" (basic_map @-> returning local_space)

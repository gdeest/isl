open Ctypes
open Foreign
open IslRaw_Types

let copy = foreign "isl_map_copy" (map @-> returning map)
let free = foreign "isl_map_free" (map @-> returning void)
let empty = foreign "isl_map_empty" (space @-> returning map)
let universe = foreign "isl_map_universe" (space @-> returning map)
let identity = foreign "isl_map_identity" (space @-> returning map)
let nat_universe = foreign "isl_map_nat_universe" (space @-> returning map)
let add_constraint = foreign "isl_map_add_constraint" (map @-> constrnt @-> returning map)

let lex_lt = foreign "isl_map_lex_lt" (space @-> returning map)
let lex_le = foreign "isl_map_lex_le" (space @-> returning map)
let lex_gt = foreign "isl_map_lex_gt" (space @-> returning map)
let lex_ge = foreign "isl_map_lex_ge" (space @-> returning map)

let lex_lt_first = foreign "isl_map_lex_lt_first" (space @-> int @-> returning map)
let lex_le_first = foreign "isl_map_lex_le_first" (space @-> int @-> returning map)
let lex_gt_first = foreign "isl_map_lex_gt_first" (space @-> int @-> returning map)
let lex_ge_first = foreign "isl_map_lex_ge_first" (space @-> int @-> returning map)

let from_basic_map = foreign "isl_map_from_basic_map" (basic_map @-> returning map)
let from_union_map = foreign "isl_map_from_union_map" (union_map @-> returning map)    

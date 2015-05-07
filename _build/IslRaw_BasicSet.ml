open Ctypes
open Foreign
open IslRaw_Types

let copy = foreign "isl_basic_set_copy" (basic_set @-> returning basic_set)
let free = foreign "isl_basic_set_free" (basic_set @-> returning void)
let empty = foreign "isl_basic_set_empty" (space @-> returning basic_set)
let universe = foreign "isl_basic_set_universe" (space @-> returning basic_set)
let nat_universe = foreign "isl_basic_set_nat_universe" (space @-> returning basic_set)
let add_constraint = foreign "isl_basic_set_add_constraint" (basic_set @-> constrnt @-> returning basic_set)
let drop_constraint = foreign "isl_basic_set_drop_constraint" (basic_set @-> constrnt @-> returning basic_set)
let get_local_space = foreign "isl_basic_set_get_local_space" (basic_set @-> returning local_space)
let remove_divs = foreign "isl_basic_set_remove_divs" (basic_set @-> returning basic_set)
let remove_divs_involving_dims = foreign "isl_basic_set_remove_divs_involving_dims"
    (basic_set @-> dim_type @-> uint @-> uint @-> returning basic_set)
let remove_unknown_divs = foreign "isl_basic_set_remove_unknown_divs" (basic_set @-> returning basic_set)
let n_constraint = foreign "isl_basic_set_n_constraint" (basic_set @-> returning int)
(* int isl_basic_set_foreach_constraint(__isl_keep isl_basic_set *bset, int (\*fn)(__isl_take constrnt *c, void *user), void *user); *)
let foreach_constraint = foreign "isl_basic_set_foreach_constraint"
    (basic_set @-> funptr (constrnt @-> ptr void @-> returning int) @-> ptr void @-> returning int)

let equalities_matrix = foreign "isl_basic_set_equalities_matrix"
    (basic_set @-> dim_type  @-> dim_type @-> dim_type @-> dim_type @-> returning matrix)
let inequalities_matrix = foreign "isl_basic_set_equalities_matrix"
    (basic_set @-> dim_type  @-> dim_type @-> dim_type @-> dim_type @-> returning matrix)

open Ctypes
open Foreign
open IslRaw_Types

(* struct isl_constraint *isl_constraint_copy(struct isl_constraint *c); *)
let copy = foreign "isl_constraint_copy" (constrnt @-> returning constrnt)
(* __isl_null isl_constraint *isl_constraint_free(__isl_take isl_constraint *c);     *)
let free = foreign "isl_constraint_free" (constrnt @-> returning void)

(* __isl_give isl_constraint *isl_equality_alloc(__isl_take isl_local_space *ls); *)
let equality = foreign "isl_equality_alloc" (local_space @-> returning constrnt)
    
(* __isl_give isl_constraint *isl_inequality_alloc(__isl_take isl_local_space *ls); *)
(* TODO: Rajouter les autres protos qui manquent... *)
let inequality = foreign "isl_inequality_alloc" (local_space @-> returning constrnt)
let set_constant_si = foreign "isl_constraint_set_constant_si" (constrnt @-> int @-> returning constrnt)
let set_constant_val = foreign "isl_constraint_set_constant_si" (constrnt @-> value @-> returning constrnt)
let set_coefficient_si = foreign "isl_constraint_set_coefficient_si" (constrnt @-> dim_type @-> int @-> int @-> returning constrnt)
let set_coefficient_val = foreign "isl_constraint_set_coefficient_si" (constrnt @-> dim_type @-> int @-> value @-> returning constrnt)
let get_local_space = foreign "isl_constraint_get_local_space" (constrnt @-> returning local_space)
let is_lower_bound = foreign "isl_constraint_is_lower_bound" (constrnt @-> dim_type @-> uint @-> returning bool)
let is_upper_bound = foreign "isl_constraint_is_upper_bound" (constrnt @-> dim_type @-> uint @-> returning bool)
let get_constant_val = foreign "isl_constraint_get_constant_val" (constrnt @-> returning value)
let get_coefficient_val = foreign "isl_constraint_get_coefficient_val" (constrnt @-> dim_type @-> int @-> returning value)
let involves_dims = foreign "isl_constraint_involves_dims" (constrnt @-> dim_type @-> uint @-> uint @-> returning bool)
let get_div = foreign "isl_constraint_get_div" (constrnt @-> int @-> returning aff)
let is_equality = foreign "isl_constraint_is_equality" (constrnt @-> returning bool)
let get_dim_name = foreign "isl_constraint_get_dim_name" (constrnt @-> dim_type @-> uint @-> returning string_opt)

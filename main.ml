open Foreign
open Ctypes

module Raw = struct

(* TODO GMP *)
(* TODO ERROR-HANDLING *)

type isl_id = unit ptr
let isl_id : isl_id typ = ptr void


(* isl_ctx *isl_id_get_ctx(__isl_keep isl_id *id); *)
(* void *isl_id_get_user(__isl_keep isl_id *id); *)
(* __isl_keep const char *isl_id_get_name(__isl_keep isl_id *id); *)

(* __isl_give isl_printer *isl_printer_print_id( *)
(* __isl_take isl_printer *p, __isl_keep isl_id *id); *)




type isl_space = unit ptr
let isl_space : isl_space typ = ptr void





type isl_local_space = unit ptr
let isl_local_space : isl_local_space typ = ptr void

type isl_basic_set = unit ptr
let isl_basic_set : isl_basic_set typ = ptr void
let isl_basic_set_copy = foreign "isl_basic_set_copy" (isl_basic_set @-> returning isl_basic_set)
let isl_basic_set_free = foreign "isl_basic_set_free" (isl_basic_set @-> returning void)
let isl_basic_set_empty = foreign "isl_basic_set_empty" (isl_space @-> returning isl_basic_set)
let isl_basic_set_universe = foreign "isl_basic_set_universe" (isl_space @-> returning isl_basic_set)
let isl_basic_set_nat_universe = foreign "isl_basic_set_nat_universe" (isl_space @-> returning isl_basic_set)
let isl_basic_set_add_constraint = foreign "isl_basic_set_add_constraint" (isl_basic_set @-> isl_constraint @-> returning isl_basic_set)
let isl_basic_set_drop_constraint = foreign "isl_basic_set_drop_constraint" (isl_basic_set @-> isl_constraint @-> returning isl_basic_set)
let isl_basic_set_get_local_space = foreign "isl_basic_set_get_local_space" (isl_basic_set @-> returning isl_local_space)
let isl_basic_set_remove_divs = foreign "isl_basic_set_remove_divs" (isl_basic_set @-> returning isl_basic_set)
let isl_basic_set_remove_divs_involving_dims = foreign "isl_basic_set_remove_divs_involving_dims"
    (isl_basic_set @-> isl_dim_type @-> uint @-> uint @-> returning isl_basic_set)
let isl_basic_set_remove_unknown_divs = foreign "isl_basic_set_remove_unknown_divs" (isl_basic_set @-> returning isl_basic_set)
let isl_basic_set_n_constraint = foreign "isl_basic_set_n_constraint" (isl_basic_set @-> returning int)
(* int isl_basic_set_foreach_constraint(__isl_keep isl_basic_set *bset, int (\*fn)(__isl_take isl_constraint *c, void *user), void *user); *)
let isl_basic_set_foreach_constraint = foreign "isl_basic_set_foreach_constraint"
    (isl_basic_set @-> funptr (isl_constraint @-> ptr void @-> returning int) @-> ptr void @-> returning int)

let isl_basic_set_equalities_matrix = foreign "isl_basic_set_equalities_matrix"
    (isl_basic_set @-> isl_dim_type  @-> isl_dim_type @-> isl_dim_type @-> isl_dim_type @-> returning isl_mat)
let isl_basic_set_inequalities_matrix = foreign "isl_basic_set_equalities_matrix"
    (isl_basic_set @-> isl_dim_type  @-> isl_dim_type @-> isl_dim_type @-> isl_dim_type @-> returning isl_mat)


type isl_set = unit ptr
let isl_set : isl_set typ = ptr void
let isl_set_copy = foreign "isl_set_copy" (isl_set @-> returning isl_basic_set)
let isl_set_free = foreign "isl_set_free" (isl_set @-> returning void)
let isl_set_empty = foreign "isl_set_empty" (isl_space @-> returning isl_set)
let isl_set_universe = foreign "isl_set_universe" (isl_space @-> returning isl_set)
let isl_set_nat_universe = foreign "isl_set_nat_universe" (isl_space @-> returning isl_set)
let isl_set_add_constraint = foreign "isl_set_add_constraint" (isl_set @-> isl_constraint @-> returning isl_set)
let isl_set_from_basic_set = foreign "isl_set_from_basic_set" (isl_basic_set @-> returning isl_set)


type isl_union_set = unit ptr
let isl_union_set : isl_union_set typ = ptr void
let isl_union_set_copy = foreign "isl_union_set_copy" (isl_union_set @-> returning isl_union_set)
let isl_union_set_free = foreign "isl_union_set_free" (isl_union_set @-> returning void)
let isl_union_set_empty = foreign "isl_union_set_empty" (isl_space @-> returning isl_union_set)
let isl_union_set_universe = foreign "isl_union_set_universe" (isl_space @-> returning isl_union_set)
let isl_union_set_from_set = foreign "isl_union_set_from_set" (isl_set @-> returning isl_union_set)    
let isl_set_from_union_set = foreign "isl_set_from_union_set" (isl_union_set @-> returning isl_set)

type isl_basic_map = unit ptr
let isl_basic_map : isl_basic_map typ = ptr void
let isl_basic_map_copy = foreign "isl_basic_map_copy" (isl_basic_map @-> returning isl_basic_map)
let isl_basic_map_free = foreign "isl_basic_map_free" (isl_basic_map @-> returning void)
let isl_basic_map_empty = foreign "isl_basic_map_empty" (isl_space @-> returning isl_basic_map)
let isl_basic_map_universe = foreign "isl_basic_map_universe" (isl_space @-> returning isl_basic_map)
let isl_basic_map_identity = foreign "isl_basic_map_identity" (isl_space @-> returning isl_basic_map)
let isl_basic_map_nat_universe = foreign "isl_basic_map_nat_universe" (isl_space @-> returning isl_basic_map)
let isl_basic_map_add_constraint = foreign "isl_basic_map_add_constraint" (isl_basic_map @-> isl_constraint @-> returning isl_basic_map)
let isl_basic_map_get_local_space = foreign "isl_basic_map_get_local_space" (isl_basic_map @-> returning isl_local_space)

type isl_map = unit ptr
let isl_map : isl_map typ = ptr void
let isl_map_copy = foreign "isl_map_copy" (isl_map @-> returning isl_map)
let isl_map_free = foreign "isl_map_free" (isl_map @-> returning void)
let isl_map_empty = foreign "isl_map_empty" (isl_space @-> returning isl_map)
let isl_map_universe = foreign "isl_map_universe" (isl_space @-> returning isl_map)
let isl_map_identity = foreign "isl_map_identity" (isl_space @-> returning isl_map)
let isl_map_nat_universe = foreign "isl_map_nat_universe" (isl_space @-> returning isl_map)
let isl_map_add_constraint = foreign "isl_map_add_constraint" (isl_map @-> isl_constraint @-> returning isl_map)

let isl_map_lex_lt = foreign "isl_map_lex_lt" (isl_space @-> returning isl_map)
let isl_map_lex_le = foreign "isl_map_lex_le" (isl_space @-> returning isl_map)
let isl_map_lex_gt = foreign "isl_map_lex_gt" (isl_space @-> returning isl_map)
let isl_map_lex_ge = foreign "isl_map_lex_ge" (isl_space @-> returning isl_map)

let isl_map_lex_lt_first = foreign "isl_map_lex_lt_first" (isl_space @-> int @-> returning isl_map)
let isl_map_lex_le_first = foreign "isl_map_lex_le_first" (isl_space @-> int @-> returning isl_map)
let isl_map_lex_gt_first = foreign "isl_map_lex_gt_first" (isl_space @-> int @-> returning isl_map)
let isl_map_lex_ge_first = foreign "isl_map_lex_ge_first" (isl_space @-> int @-> returning isl_map)

let isl_map_from_basic_map = foreign "isl_map_from_basic_map" (isl_basic_map @-> returning isl_map)


type isl_union_map = unit ptr
let isl_union_map : isl_union_map typ = ptr void
let isl_union_map_copy = foreign "isl_union_map_copy" (isl_union_map @-> returning isl_union_map)
let isl_union_map_free = foreign "isl_union_map_free" (isl_union_map @-> returning void)
let isl_union_map_empty = foreign "isl_union_map_empty" (isl_space @-> returning isl_union_map)
let isl_union_map_universe = foreign "isl_union_map_universe" (isl_space @-> returning isl_union_map)
let isl_union_map_from_map = foreign "isl_union_map_from_map" (isl_map @-> returning isl_union_map)
let isl_map_from_union_map = foreign "isl_map_from_union_map" (isl_union_map @-> returning isl_map)


(* Printing functions *)
type isl_printer = unit ptr
let isl_printer : isl_printer typ = ptr void

let isl_printer_to_str = foreign "isl_printer_to_str" (isl_ctx @-> returning isl_printer)
let isl_printer_free = foreign "isl_printer_free" (isl_printer @-> returning void)
let isl_printer_get_str = foreign "isl_printer_get_str" (isl_printer @-> returning string)

let isl_printer_print_space = foreign "isl_printer_print_space" (isl_printer @-> isl_space @-> returning isl_printer)
let isl_printer_print_basic_set = foreign "isl_printer_print_basic_set" (isl_printer @-> isl_basic_set @-> returning isl_printer)
let isl_printer_print_basic_map = foreign "isl_printer_print_basic_map" (isl_printer @-> isl_basic_map @-> returning isl_printer)
let isl_printer_print_set = foreign "isl_printer_print_set" (isl_printer @-> isl_set @-> returning isl_printer)
let isl_printer_print_map = foreign "isl_printer_print_map" (isl_printer @-> isl_map @-> returning isl_printer)        


let print_with ctx f o =
  let printer = isl_printer_to_str ctx in
  let printer = f printer o in
  let str = isl_printer_get_str printer in
  isl_printer_free printer; str

end

module HighLevel () : sig
  type id
  type value
  type space
  type local_space
  type aff
  type pw_aff
  type multi_aff
  type pw_multi_aff
  type matrix
  type constrnt_type = Equality | Inequality
  type constrnt
  type bset
  type set
  type uset
  type bmap
  type map
  type umap
  type dim_type = [ `Param | `In | `Out | `Set ]
  type dim_type_noparam = [ `In | `Out | `Set ]                        
    
    
  module Val : sig
    type t = value
      
    val zero : t
    val one : t
    val negone : t
    val nan : t
    val infty : t
    val neginfty : t
      
    val num : t -> int64
    val den : t -> int64

    val sgn : t -> int
    val is_zero : t -> bool
    val is_one : t -> bool
    val is_negone : t -> bool
    val is_nonneg : t -> bool
    val is_nonpos : t -> bool
    val is_pos : t -> bool
    val is_neg : t -> bool
    val is_int : t -> bool
    val is_rat : t -> bool
    val is_nan : t -> bool
    val is_infty : t -> bool
    val is_neginfty : t -> bool

    val lt : t -> t -> bool
    val le : t -> t -> bool
    val gt : t -> t -> bool
    val ge : t -> t -> bool      
    val eq : t -> t -> bool
    val ne : t -> t -> bool
    val divisible : t -> t -> bool
    val cmp_int64 : t -> int64 -> int
      
    val (<) : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>) : t -> t -> bool
    val (>=) : t -> t -> bool
    val (=) : t -> t -> bool
    val (<>) : t -> t -> bool      

    val abs : t -> t
    val neg : t -> t
    val floor : t -> t
    val ceil : t -> t
    val trunc : t -> t
    val twoexp : t -> t

    val min : t -> t -> t
    val max : t -> t -> t
    val add : t -> t -> t
    val add_int : t -> int -> t
    val sub : t -> t -> t
    val sub_int : t -> int -> t
    val mul : t -> t -> t
    val mul_int : t -> int -> t
    val div : t -> t -> t

    val modulo : t -> t -> t
    val gcd : t -> t -> t
    val gcdext : t -> t -> (t * t * t)
    
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val (~-) : t -> t
    val ( * ) : t -> t -> t
    val (mod) : t -> t -> t
    val (/) : t -> t -> t       

    val of_int64 : int64 -> t
    val of_int : int -> t
    val to_int64 : t -> int64 option    
  end

  module Id : sig
    type t = id

    val alloc : string -> t
  end
  
  module Space : sig
    type t = space

    val alloc : nparam:int -> nin:int -> nout:int -> t
    val params_alloc : nparam:int -> t
    val set_alloc : nparam:int -> ndim:int -> t

    val num_dims : t -> dim_type -> int
    
    val is_params : t -> bool
    val is_set : t -> bool
    val is_map : t -> bool

    val is_equal : t -> t -> bool
    val is_domain : t -> t -> bool
    val is_range : t -> t -> bool
    val tuple_is_equal : t -> dim_type -> t -> dim_type -> bool

    val set_dim_id : t -> dim_type -> int -> Id.t -> t
    val has_dim_id : t -> dim_type -> int -> bool
    val get_dim_id : t -> dim_type -> int -> Id.t option
    val set_dim_name : t -> dim_type -> int -> string -> t
    val has_dim_name : t -> dim_type -> int -> bool
    val get_dim_name : t -> dim_type -> int -> string option

    val find_dim_by_id : t -> dim_type -> Id.t -> int option
    val find_dim_by_name : t -> dim_type -> string -> int option

    val set_tuple_id : t -> dim_type_noparam -> Id.t -> t
    val reset_tuple_id : t -> dim_type_noparam -> t
    val has_tuple_id : t -> dim_type_noparam -> bool
    val get_tuple_id : t -> dim_type_noparam -> Id.t option
    val set_tuple_name : t -> dim_type_noparam -> string -> t
    val has_tuple_name : t -> dim_type_noparam -> bool
    val get_tuple_name : t -> dim_type_noparam -> string option

    val domain : t -> t
    val from_domain : t -> t
    val range : t -> t
    val from_range : t -> t
    val domain_map : t -> t
    val range_map : t -> t
    val params : t -> t
    val set_from_params : t -> t
    val reverse : t -> t
    val join : t -> t -> t
    val align_params : t -> t -> t
    val insert_dims : t -> dim_type -> at:int -> n:int -> t
    val add_dims : t -> dim_type -> n:int -> t
    val drop_dims : t -> dim_type -> at:int -> n:int -> t
    val move_dims : t -> to_tuple:dim_type -> to_pos:int -> from_tuple:dim_type -> from_pos:int -> n:int -> t

    val map_from_set : t -> t
    val map_from_domain_and_range : t -> t -> t
    val zip : t -> t
    val curry : t -> t
    val uncurry : t -> t
    
    val to_string : t -> string
  end

  module LocalSpace : sig
    type t = local_space

    val from_space : space -> t
  end

  module Aff : sig
    type t = aff
  end

  module PwAff : sig
    type t = pw_aff
  end    
  
  module MultiAff : sig
    type t = multi_aff
  end

  module PwMultiAff : sig
    type t = pw_multi_aff
  end

  module Matrix : sig
    type t = matrix
  end
  
  module Constraint : sig
    type t = constrnt

    val equality_alloc : local_space -> t
    val inequality_alloc : local_space -> t
    val set_constant_si : t -> int -> t
    val set_constant_val : t -> value -> t
    val set_coefficient_si : t -> dim_type -> int -> int -> t
    val set_coefficient_val : t -> dim_type -> int -> value -> t
    val local_space : t -> local_space

    val is_lower_bound : t -> dim_type -> int -> bool
    val is_upper_bound : t -> dim_type -> int -> bool
    val get_constant_val : t -> value
    val get_coefficient_val : t -> dim_type -> int -> value
    val involves_dims : t -> dim_type -> first:int -> n:int -> bool

    val get_div : t -> int -> aff

    (* À implémenter avec is_equality_constraint *)
    val get_type : t -> constrnt_type

    val get_dim_name : t -> dim_type -> int -> string option
  end

  module BasicSet : sig
    type t = bset
    val empty : Space.t -> t
    val universe : Space.t -> t
    val nat_universe : Space.t -> t
    val add_constraint : t -> constrnt -> t
    val drop_constraint : t -> constrnt -> t
    val local_space : t -> local_space

    val remove_divs : t -> t
    val remove_divs_involving_dims : t -> dim_type -> first:int -> n:int -> t
    val remove_unknown_divs : t -> t

    val n_constraint : t -> int
    val foreach_constraint : t -> (constrnt -> bool) -> unit

    val equalities_matrix : t -> dim_type -> dim_type -> dim_type -> dim_type -> matrix
    val inequalities_matrix : t -> dim_type -> dim_type -> dim_type -> dim_type -> matrix

    (* val ndim : t -> dim_type -> int *)
    (* val involves_dims : t -> dim_type -> first:int -> n:int -> bool *)

    (* val set_tuple_id : t -> id -> t *)
    (* val get_tuple_name : t -> string option *)
    (* val set_tuple_name : t -> string -> t *)

    (* val get_dim_id : t -> dim_type -> int -> id option *)
    (* val get_dim_name : t -> dim_type -> int -> string option *)
    
    val to_string : t -> string
  end

  module BasicMap : sig
    type t = bmap
    val empty : Space.t -> t
    val universe : Space.t -> t
    val nat_universe : Space.t -> t
    val identity : Space.t -> t
    val add_constraint : t -> constrnt -> t
    val local_space : t -> local_space      

    (* val remove_divs : t -> t *)
    (* val remove_divs_involving_dims : t -> dim_type -> first:int -> n:int -> t *)
    (* (\* TODO: Check that it does not exist. *\) *)
    (* (\* val remove_unknown_divs : t -> t *\) *)

    (* val n_constraint : t -> int *)
    (* val foreach_constraint : t -> (constrnt -> unit) -> unit     *)

    (* val equalities_matrix : t -> dim_type -> dim_type -> dim_type -> dim_type -> matrix *)
    (* val inequalities_matrix : t -> dim_type -> dim_type -> dim_type -> dim_type -> matrix *)

    (* val ndim : t -> dim_type -> int *)
    (* val involves_dims : t -> dim_type -> first:int -> n:int -> bool *)

    (* val set_tuple_id : t -> dim_type -> id -> t *)
    (* val get_tuple_name : t -> dim_type -> string option *)
    (* val set_tuple_name : t -> dim_type -> string -> t *)

    (* val has_dim_id : t -> dim_type -> int -> bool *)
    (* val get_dim_name : t -> dim_type -> int -> string option *)
    
    (* val to_string : t -> string *)
  end

  module Set : sig
    type t = set
    val empty : Space.t -> t
    val universe : Space.t -> t
    val nat_universe : Space.t -> t
    val from_basic_set : BasicSet.t -> t
    val from_union_set : uset -> t
    val add_constraint : t -> constrnt -> t

    (* val compute_divs : t -> t *)
    (* val align_divs : t -> t *)
    (* val remove_divs : t -> t *)
    (* val remove_divs_involving_dims : t -> dim_type -> first:int -> n:int -> t             *)
    (* val remove_unknown_divs : t -> t *)

    (* val n_basic_set : t -> int *)
    (* val foreach_basic_set : t -> (bset -> unit) -> unit *)
    (* val make_disjoint : t -> t *)

    (* val ndim : t -> dim_type -> int *)
    (* val involves_dims : t -> dim_type -> first:int -> n:int -> bool *)
    (* val dim_has_any_lower_bound : t -> dim_type -> int -> bool *)
    (* val dim_has_any_upper_bound : t -> dim_type -> int -> bool       *)
    (* val dim_has_lower_bound : t -> dim_type -> int -> bool *)
    (* val dim_has_upper_bound : t -> dim_type -> int -> bool *)

    (* val set_tuple_id : t -> id -> t *)
    (* val reset_tuple_id : t -> t *)
    (* val has_tuple_id : t -> bool *)
    (* val get_tuple_id : t -> id option *)
    (* val has_tuple_name : t -> bool *)
    (* val set_tuple_name : t -> string -> t *)
    (* val get_tuple_name : t -> string option *)

    (* val set_dim_id : t -> dim_type -> int -> id -> t *)
    (* val has_dim_id : t -> dim_type -> int -> bool *)
    (* val get_dim_id : t -> dim_type -> int -> id option *)
    (* val has_dim_name : t -> dim_type -> int -> bool *)
    (* val get_dim_name : t -> dim_type -> int -> string option *)

    (* val find_dim_by_id : t -> dim_type -> id -> int option *)
    (* val find_dim_by_name : t -> dim_type -> string -> int option         *)
    
    (* val to_string : t -> string *)
  end

  module Map : sig
    type t = map
    val empty : Space.t -> t
    val universe : Space.t -> t
    val nat_universe : Space.t -> t
    val identity : Space.t -> t
    val lex_lt : Space.t -> t
    val lex_le : Space.t -> t
    val lex_gt : Space.t -> t
    val lex_ge : Space.t -> t
    val lex_lt_first : Space.t -> int -> t
    val lex_le_first : Space.t -> int -> t
    val lex_gt_first : Space.t -> int -> t
    val lex_ge_first : Space.t -> int -> t
    val from_basic_map : BasicMap.t -> t
    val from_union_map : umap -> t
    val add_constraint : t -> constrnt -> t

    (* val compute_divs : t -> t *)
    (* val align_divs : t -> t *)
    (* val remove_divs : t -> t *)
    (* val remove_divs_involving_dims : t -> dim_type -> first:int -> n:int -> t *)
    (* val remove_unknown_divs : t -> t *)

    (* val n_basic_map : t -> int     *)
    (* val foreach_basic_map : t -> (bmap -> unit) -> unit *)
    (* val make_disjoint : t -> t       *)

    (* val ndim : t -> dim_type -> int *)
    (* val involves_dims : t -> dim_type -> first:int -> n:int -> bool       *)

    (* val set_tuple_id : t -> dim_type -> id -> t *)
    (* val reset_tuple_id : t -> dim_type -> t *)
    (* val has_tuple_id : t -> dim_type -> bool *)
    (* val get_tuple_id : t -> dim_type -> id option *)
    (* val has_tuple_name : t -> dim_type -> bool *)
    (* val get_tuple_name : t -> dim_type -> string option *)
    (* val set_tuple_name : t -> dim_type -> string -> t *)

    (* val set_dim_id : t -> dim_type -> int -> id -> t *)
    (* val has_dim_id : t -> dim_type -> int -> bool *)
    (* val get_dim_id : t -> dim_type -> int -> id option *)
    (* val has_dim_name : t -> dim_type -> int -> bool *)
    (* val get_dim_name : t -> dim_type -> int -> string option       *)

    (* val find_dim_by_id : t -> dim_type -> id -> int option *)
    (* val find_dim_by_name : t -> dim_type -> string -> int option             *)
    
    val to_string : t -> string
  end

  module UnionSet : sig
    type t = uset
    val empty : Space.t -> t
    val universe : Space.t -> t
    val from_set : Set.t -> t

    (* val n_set : t -> int *)
    (* val foreach_set : t -> (set -> unit) -> unit *)
    (* val extract_set : t -> space -> set *)
  end

  module UnionMap : sig
    type t = umap
    val empty : Space.t -> t
    val universe : Space.t -> t
    val from_map : Map.t -> t

    (* val n_map : t -> int     *)
    (* val foreach_map : t -> (t -> unit) -> unit *)
    (* val extract_map : t -> space -> map *)

    (* val get_dim_id : t -> dim_type -> int -> id option *)
  end
end = struct
  open Raw

  type id = Raw.isl_id
  type value = Raw.isl_val
  type space = Raw.isl_space
  type local_space = Raw.isl_local_space
  type aff = isl_aff
  type pw_aff
  type multi_aff
  type pw_multi_aff
  type matrix = isl_mat
  type constrnt_type = Equality | Inequality                       
  type constrnt = Raw.isl_constraint
  type bset = Raw.isl_basic_set
  type set = Raw.isl_set
  type uset = Raw.isl_union_set
  type bmap = Raw.isl_basic_map
  type map = Raw.isl_map
  type umap = Raw.isl_union_map
  
  type dim_type = [ `Param | `In | `Out | `Set ]
  type dim_type_noparam = [ `In | `Out | `Set ]
  let to_isl_dim_type = function
    | `Param -> DimParam
    | `In -> DimIn
    | `Out -> DimOut
    | `Set -> DimSet
  
  let ctx = isl_ctx_alloc ()
      
  let wrap free v =
    Gc.finalise free v; v
  
  module Val = struct
    type t = isl_val

    let wrap = wrap isl_val_free
    let copy = isl_val_copy
        
    let zero = wrap @@ isl_val_zero ctx        
    let one = wrap @@ isl_val_one ctx
    let negone = wrap @@ isl_val_negone ctx
    let nan = wrap @@ isl_val_nan ctx
    let infty = wrap @@ isl_val_infty ctx
    let neginfty = wrap @@ isl_val_neginfty ctx

    let num v = Signed.Long.to_int64 @@ isl_val_get_num_si v
    let den v = Signed.Long.to_int64 @@ isl_val_get_den_si v

    let sgn = isl_val_sgn
    let is_zero = isl_val_is_zero
    let is_one = isl_val_is_one
    let is_negone = isl_val_is_negone
    let is_nonneg = isl_val_is_nonneg
    let is_nonpos = isl_val_is_nonpos
    let is_pos = isl_val_is_pos
    let is_neg = isl_val_is_neg
    let is_int = isl_val_is_int
    let is_rat = isl_val_is_rat
    let is_nan = isl_val_is_nan
    let is_infty = isl_val_is_infty
    let is_neginfty = isl_val_is_neginfty

    let lt = isl_val_lt
    let le = isl_val_le
    let gt = isl_val_gt
    let ge = isl_val_ge
    let eq = isl_val_eq
    let ne = isl_val_ne
    let divisible = isl_val_is_divisible_by
    let cmp_int64 v i = isl_val_cmp_si v (Signed.Long.of_int64 i)
    let (<), (<=), (>), (>=), (=), (<>) = lt, le, gt, ge, eq, ne        

    let abs v = wrap @@ isl_val_abs @@ copy v
    let neg v = wrap @@ isl_val_neg @@ copy v
    let floor v = wrap @@ isl_val_floor @@ copy v
    let ceil v = wrap @@ isl_val_ceil @@ copy v
    let trunc v = wrap @@ isl_val_trunc @@ copy v
    let twoexp v = wrap @@ isl_val_2exp @@ copy v

    let min v1 v2 = wrap @@ isl_val_min (copy v1) (copy v2)
    let max v1 v2 = wrap @@ isl_val_max (copy v1) (copy v2)        
    let add v1 v2 = wrap @@ isl_val_add (copy v1) (copy v2)
    let add_int v i = wrap @@ isl_val_add_ui (copy v) (Unsigned.ULong.of_int i)
    let sub v1 v2 = wrap @@ isl_val_sub (copy v1) (copy v2)
    let sub_int v i = wrap @@ isl_val_sub_ui (copy v) (Unsigned.ULong.of_int i)
    let mul v1 v2 = wrap @@ isl_val_mul (copy v1) (copy v2)
    let mul_int v i = wrap @@ isl_val_mul_ui (copy v) (Unsigned.ULong.of_int i)
    let div v1 v2 = wrap @@ isl_val_div (copy v1) (copy v2)
        
    let modulo a b = wrap @@ isl_val_mod (copy a) (copy b)
    let gcd a b = wrap @@ isl_val_gcd (copy a) (copy b)
    let gcdext a b =
      let x_ptr = allocate isl_val null in
      let y_ptr = allocate isl_val null in
      let d = wrap @@ isl_val_gcdext a b x_ptr y_ptr in
      (d, wrap !@x_ptr, wrap !@y_ptr)
      
    let (+), (-), (~-), ( * ), (mod), (/) = add, sub, neg, mul, modulo, div

    let of_int64 i =
      let i = Signed.Long.of_int64 i in
      wrap @@ isl_val_int_from_si ctx i
        
    let of_int i = of_int64 @@ Int64.of_int i
    let to_int64 v = if (is_int v) then Some (num v) else None  
  end

  module Id = struct
    type t = Raw.isl_id

    let copy = isl_id_copy
    let wrap = wrap isl_id_free
    let alloc name = wrap @@ isl_id_alloc ctx name null
  end
  
  module Space = struct
    type t = Raw.isl_space
                                
    let wrap = wrap isl_space_free
    let copy = isl_space_copy

    let of_int = Unsigned.UInt.of_int

    let alloc ~nparam ~nin ~nout = wrap @@ isl_space_alloc ctx (of_int nparam) (of_int nin) (of_int nout)
    let params_alloc ~nparam = wrap @@ isl_space_params_alloc ctx (of_int nparam)
    let set_alloc ~nparam ~ndim = wrap @@ isl_space_set_alloc ctx (of_int nparam) (of_int ndim)

    let num_dims space dim_type = Unsigned.UInt.to_int @@
      isl_space_dim space (to_isl_dim_type dim_type)
    
    let is_map = isl_space_is_map
    let is_set = isl_space_is_set
    let is_params = isl_space_is_params

    let is_equal = isl_space_is_equal
    let is_domain = isl_space_is_domain
    let is_range = isl_space_is_range
    let tuple_is_equal s1 t1 s2 t2 = isl_space_tuple_is_equal s1 (to_isl_dim_type t1) s2 (to_isl_dim_type t2)
    
    let set_dim_id s dim_type pos id =  wrap @@
      isl_space_set_dim_id (copy s) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos) id
    let has_dim_id s dim_type pos = isl_space_has_dim_id s (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)
    let get_dim_id s dim_type pos =
      if has_dim_id s dim_type pos then
        Some (isl_space_get_dim_id s (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos))
      else None
    let set_dim_name s dim_type pos name =  wrap @@
      isl_space_set_dim_name (copy s) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos) name
    let has_dim_name s dim_type pos = isl_space_has_dim_name s (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)
    let get_dim_name s dim_type pos = isl_space_get_dim_name s (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)

    let find_dim_by_id s dim_type id =
      let pos = isl_space_find_dim_by_id s (to_isl_dim_type dim_type) id in
      if (pos < 0) then None else Some pos
          
    let find_dim_by_name s dim_type name =
      let pos = isl_space_find_dim_by_name s (to_isl_dim_type dim_type) name in
      if (pos < 0) then None else Some pos          

    let set_tuple_id s dim_type id = wrap @@
      isl_space_set_tuple_id (copy s) (to_isl_dim_type dim_type) (Id.copy id)

    let reset_tuple_id s dim_type = wrap @@
      isl_space_reset_tuple_id (copy s) (to_isl_dim_type dim_type)

    let has_tuple_id s dim_type = isl_space_has_tuple_id s (to_isl_dim_type dim_type)
    let get_tuple_id s dim_type =
      if has_tuple_id s dim_type then
        Some (isl_space_get_tuple_id s (to_isl_dim_type dim_type))
      else None

    let set_tuple_name s dim_type name = wrap @@
      isl_space_set_tuple_name (copy s) (to_isl_dim_type dim_type) name

    let has_tuple_name s dim_type = isl_space_has_tuple_name s (to_isl_dim_type dim_type)
    let get_tuple_name s dim_type = isl_space_get_tuple_name s (to_isl_dim_type dim_type)

    let domain s = wrap @@ isl_space_domain (copy s)
    let from_domain s = wrap @@ isl_space_from_domain (copy s)
    let range s = wrap @@ isl_space_range (copy s)
    let from_range s = wrap @@ isl_space_from_range (copy s)
    let domain_map s = wrap @@ isl_space_domain_map (copy s)
    let range_map s = wrap @@ isl_space_range_map (copy s)
    let params s = wrap @@ isl_space_params (copy s)
    let set_from_params s = wrap @@ isl_space_set_from_params (copy s)
    let reverse s = wrap @@ isl_space_reverse (copy s)
    let join s1 s2 = wrap @@ isl_space_join (copy s1) (copy s2)
    let align_params s1 s2 = wrap @@ isl_space_join (copy s1) (copy s2)
    let insert_dims s dim_type ~at ~n = wrap @@
      isl_space_insert_dims (copy s) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int at) (Unsigned.UInt.of_int n)
    let add_dims s dim_type ~n = wrap @@
      isl_space_add_dims (copy s) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int n)
    let drop_dims s dim_type ~at ~n = wrap @@
      isl_space_drop_dims (copy s) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int at) (Unsigned.UInt.of_int n)
    let move_dims s ~to_tuple ~to_pos ~from_tuple ~from_pos ~n = wrap @@
      isl_space_move_dims (copy s) (to_isl_dim_type to_tuple) (Unsigned.UInt.of_int to_pos) (to_isl_dim_type from_tuple) (Unsigned.UInt.of_int from_pos) (Unsigned.UInt.of_int n)

    let map_from_set s = wrap @@ isl_space_map_from_set (copy s)
    let map_from_domain_and_range domain range = wrap @@ isl_space_map_from_domain_and_range (copy domain) (copy range)
    let zip s = wrap @@ isl_space_zip (copy s)
    let curry s = wrap @@ isl_space_curry (copy s)
    let uncurry s = wrap @@ isl_space_uncurry (copy s)

    let to_string = print_with ctx isl_printer_print_space
    (* let to_string s = *)
    (*   let printer = isl_printer_to_str ctx in *)
    (*   let printer = isl_printer_print_space printer s in *)
    (*   let s = isl_printer_get_str printer in *)
    (*   isl_printer_free printer; s *)
  end

  module LocalSpace = struct
    type t = local_space

    let copy = isl_local_space_copy
    let wrap = wrap isl_local_space_free

    let from_space s = wrap @@ isl_local_space_from_space (Space.copy s)
  end

  module Aff = struct
    type t = aff

    let wrap = wrap isl_aff_free
    let copy = isl_aff_copy
  end

  module PwAff = struct
    type t = pw_aff
  end    
  
  module MultiAff = struct
    type t = multi_aff
  end

  module PwMultiAff = struct
    type t = pw_multi_aff
  end

  module Matrix = struct
    type t = matrix

    let wrap = wrap isl_mat_free
    let copy = isl_mat_copy
  end
  
  module Constraint = struct
    type t = constrnt

    let copy = isl_constraint_copy
    let wrap = wrap isl_constraint_free
    
    let equality_alloc s = wrap @@ isl_equality_alloc (LocalSpace.copy s)
    let inequality_alloc s = wrap @@ isl_inequality_alloc (LocalSpace.copy s)
    let set_constant_si c i = wrap @@ isl_constraint_set_constant_si (copy c) i
    let set_constant_val c v = wrap @@ isl_constraint_set_constant_val (copy c) (Val.copy v)
    let set_coefficient_si c dim_type pos i = wrap @@ isl_constraint_set_coefficient_si
        (copy c) (to_isl_dim_type dim_type) pos i
    let set_coefficient_val c dim_type pos v = wrap @@ isl_constraint_set_coefficient_val
        (copy c) (to_isl_dim_type dim_type) pos (Val.copy v)

    let local_space c = LocalSpace.wrap @@ isl_constraint_get_local_space c

    let is_lower_bound c dim_type pos = isl_constraint_is_lower_bound c (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)
    let is_upper_bound c dim_type pos = isl_constraint_is_upper_bound c (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)

    let get_constant_val c = Val.wrap @@ isl_constraint_get_constant_val c
    let get_coefficient_val c dim_type pos = Val.wrap @@ isl_constraint_get_coefficient_val c (to_isl_dim_type dim_type) pos
    let get_dim_name c dim_type pos = isl_constraint_get_dim_name c (to_isl_dim_type dim_type) (Unsigned.UInt.of_int pos)
        
    let get_type c = if isl_constraint_is_equality c then Equality else Inequality
    let get_div c pos = Aff.wrap @@ isl_constraint_get_div c pos

    let involves_dims c dim_type ~first ~n = isl_constraint_involves_dims c (to_isl_dim_type dim_type) (Unsigned.UInt.of_int first) (Unsigned.UInt.of_int n)
  end
  
  module BasicSet = struct
    type t = bset

    let copy = isl_basic_set_copy
    let wrap = wrap isl_basic_set_free

    let empty s = wrap @@ isl_basic_set_empty (Space.copy s)
    let universe s = wrap @@ isl_basic_set_universe (Space.copy s)
    let nat_universe s = wrap @@ isl_basic_set_nat_universe (Space.copy s)
    let add_constraint bs c = wrap @@ isl_basic_set_add_constraint
        (copy bs) (Constraint.copy c)
    let drop_constraint bs c = wrap @@ isl_basic_set_drop_constraint
        (copy bs) (Constraint.copy c)
    let local_space bs = LocalSpace.wrap @@ isl_basic_set_get_local_space bs

    let remove_divs bs = wrap @@ isl_basic_set_remove_divs (copy bs)
    let remove_divs_involving_dims bs dim_type ~first ~n =
      wrap @@ isl_basic_set_remove_divs_involving_dims (copy bs) (to_isl_dim_type dim_type) (Unsigned.UInt.of_int first) (Unsigned.UInt.of_int n)
    let remove_unknown_divs bs = wrap @@ isl_basic_set_remove_unknown_divs (copy bs)

    let n_constraint = isl_basic_set_n_constraint
    let foreach_constraint bs f =
      let f c _ = if (f c) then 0 else (-1) in
      let _ = isl_basic_set_foreach_constraint bs f null in ()

    let equalities_matrix bs dt1 dt2 dt3 dt4 = Matrix.wrap @@
      let tdt = to_isl_dim_type in 
      isl_basic_set_equalities_matrix bs (tdt dt1) (tdt dt2) (tdt dt3) (tdt dt4)

    let inequalities_matrix bs dt1 dt2 dt3 dt4 = Matrix.wrap @@
      let tdt = to_isl_dim_type in 
      isl_basic_set_inequalities_matrix bs (tdt dt1) (tdt dt2) (tdt dt3) (tdt dt4)    
    
    let to_string = print_with ctx isl_printer_print_basic_set
  end

  module BasicMap = struct
    type t = bset

    let copy = isl_basic_map_copy
    let wrap = wrap isl_basic_map_free

    let empty s = wrap @@ isl_basic_map_empty (Space.copy s)
    let universe s = wrap @@ isl_basic_map_universe (Space.copy s)
    let identity s = wrap @@ isl_basic_map_identity (Space.copy s)
    let nat_universe s = wrap @@ isl_basic_map_nat_universe (Space.copy s)
    let add_constraint bs c = wrap @@ isl_basic_map_add_constraint
        (copy bs) (Constraint.copy c)

    let local_space bs = LocalSpace.wrap @@ isl_basic_map_get_local_space bs
    
    let to_string = print_with ctx isl_printer_print_basic_map    
  end

  module Set = struct
    type t = set

    let copy = isl_set_copy
    let wrap = wrap isl_set_free
    
    let empty s = wrap @@ isl_set_empty (Space.copy s)
    let universe s = wrap @@ isl_set_universe (Space.copy s)
    let nat_universe s = wrap @@ isl_set_nat_universe (Space.copy s)
    let add_constraint s c = wrap @@ isl_set_add_constraint (copy s) (Constraint.copy c)

    let from_basic_set bs = wrap @@ isl_set_from_basic_set (BasicSet.copy bs)
    let from_union_set us = wrap @@ isl_set_from_union_set (isl_union_set_copy us)        
    
    let to_string = print_with ctx isl_printer_print_set
  end

  module Map = struct
    type t = map
      
    let copy = isl_map_copy
    let wrap = wrap isl_map_free

    let empty s = wrap @@ isl_map_empty (Space.copy s)
    let universe s = wrap @@ isl_map_universe (Space.copy s)
    let nat_universe s = wrap @@ isl_map_nat_universe (Space.copy s)
    let identity s = wrap @@ isl_map_identity (Space.copy s)
        
    let lex_lt s = wrap @@ isl_map_lex_lt (Space.copy s)
    let lex_le s = wrap @@ isl_map_lex_le (Space.copy s)
    let lex_gt s = wrap @@ isl_map_lex_gt (Space.copy s)
    let lex_ge s = wrap @@ isl_map_lex_ge (Space.copy s)
    let lex_lt_first s i = wrap @@ isl_map_lex_lt_first (Space.copy s) i
    let lex_le_first s i = wrap @@ isl_map_lex_le_first (Space.copy s) i
    let lex_gt_first s i = wrap @@ isl_map_lex_gt_first (Space.copy s) i
    let lex_ge_first s i = wrap @@ isl_map_lex_ge_first (Space.copy s) i

    let from_basic_map bm = wrap @@ isl_map_from_basic_map (BasicMap.copy bm)
    let from_union_map um = wrap @@ isl_map_from_union_map (isl_union_map_copy um)

    let add_constraint m c = wrap @@ isl_map_add_constraint (copy m) (Constraint.copy c)

    let to_string = print_with ctx isl_printer_print_map
  end

  module UnionSet = struct
    type t = uset

    let copy = isl_union_set_copy
    let wrap = wrap isl_union_set_free
    
    let empty s = wrap @@ isl_union_set_empty (Space.copy s)
    let universe s = wrap @@ isl_union_set_universe (Space.copy s)
    let from_set s = wrap @@ isl_union_set_from_set (Set.copy s)
  end

  module UnionMap = struct
    type t = umap

    let copy = isl_union_map_copy
    let wrap = wrap isl_union_map_free

    let empty s = wrap @@ isl_union_map_empty (Space.copy s)
    let universe s = wrap @@ isl_union_map_universe (Space.copy s)
    let from_map s = wrap @@ isl_union_map_from_map (Map.copy s)        
  end
end

module IISL1 = HighLevel ()
module IISL2 = HighLevel ()    

let _ =
  let open IISL1 in
  let v1 = Val.of_int 32 in
  let v2 = Val.of_int 48 in
  let v3 = Val.(v1 + v2) in
  match Val.to_int64 v3 with
  | Some d -> 
    print_endline @@ Int64.to_string d
  | _ -> ()

let _ =
  let open IISL1.Space in
  let space = set_alloc ~nparam:2 ~ndim:3 in
  let space = set_dim_name space `Param 0 "M" in
  let space = set_dim_name space `Param 1 "N" in
  let space = set_dim_name space `Set 0 "x" in
  let space = set_dim_name space `Set 1 "y" in
  let space = set_dim_id space `Set 2 (IISL1.Id.alloc "z") in
  let space = set_tuple_id space `Set (IISL1.Id.alloc "BLAH") in
  print_endline @@ to_string space;
  begin match find_dim_by_name space `Param "N" with
    | Some 1 -> print_endline "Yup."
    | _ -> print_endline "Too bad !"
  end;
  begin match find_dim_by_name space `Set "N" with
    | None -> print_endline "Yup again."
    | _ -> print_endline "Of course not !"
  end;
  let space1 = set_alloc ~nparam:2 ~ndim:3 in
  let space2 = move_dims space1 ~to_tuple:`Param ~to_pos:0 ~from_tuple:`Set ~from_pos:1 ~n:2 in
  let space3 = set_tuple_name space2 `Set "P" in
  print_endline @@ to_string space1;  
  print_endline @@ to_string space2;
  print_endline @@ to_string space3;
  let open IISL1 in
  let bset = BasicSet.universe space in
  let ls = BasicSet.local_space bset in
  let c = Constraint.inequality_alloc ls in
  let c = Constraint.set_constant_si c (-2) in
  let c = Constraint.set_coefficient_si c `Set 0 3 in
  let c = Constraint.set_coefficient_si c `Set 1 (-2) in  
  let bset = BasicSet.add_constraint bset c in
  print_endline @@ BasicSet.to_string bset;
  let _ = BasicSet.foreach_constraint bset (fun c ->
      let v = Val.to_int64 @@ Constraint.get_constant_val c in
      match v with
      | Some d -> print_endline @@ Int64.to_string d; true
      | None -> print_endline "Erreur !"; false
    ) in
  let m = Map.lex_lt space in
  print_endline @@ Map.to_string m
  (* let constrnt = IISL1.Constraint.inequality_alloc in *)





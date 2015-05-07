open Isl_Types

module type CTX = sig
  val ctx : IslRaw.ctx
end

module type ID = sig
  type t
  val make : string -> t
end

module type VALUE = sig
  type t

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

module type SPACE = sig
  type t
  type id

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

  val set_dim_id : t -> dim_type -> int -> id -> t
  val has_dim_id : t -> dim_type -> int -> bool
  val get_dim_id : t -> dim_type -> int -> id option
  val set_dim_name : t -> dim_type -> int -> string -> t
  val has_dim_name : t -> dim_type -> int -> bool
  val get_dim_name : t -> dim_type -> int -> string option

  val find_dim_by_id : t -> dim_type -> id -> int option
  val find_dim_by_name : t -> dim_type -> string -> int option

  val set_tuple_id : t -> noparam -> id -> t
  val reset_tuple_id : t -> noparam -> t
  val has_tuple_id : t -> noparam -> bool
  val get_tuple_id : t -> noparam -> id option
  val set_tuple_name : t -> noparam -> string -> t
  val has_tuple_name : t -> noparam -> bool
  val get_tuple_name : t -> noparam -> string option

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

module type LOCAL_SPACE = sig
  type t
  type space

  val from_space : space -> t
end

module type CONSTRAINT = sig
  type t
  type value
  type local_space
  type aff

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

module type AFF = sig
  type t
end

module type MATRIX = sig
  type t
end

module type BASIC_SET = sig
  type t
  type space
  type local_space
  type matrix
  type constrnt
    
  val empty : space -> t
  val universe : space -> t
  val nat_universe : space -> t
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

module type BASIC_MAP = sig
  type t
  type space
  type local_space
  type constrnt
    
  val empty : space -> t
  val universe : space -> t
  val nat_universe : space -> t
  val identity : space -> t
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

module type SET = sig
  type t
  type space
  type basic_set
  type union_set
  type constrnt 
    
  val empty : space -> t
  val universe : space -> t
  val nat_universe : space -> t
  val from_basic_set : basic_set -> t
  val from_union_set : union_set -> t
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

  val to_string : t -> string
end

module type MAP = sig
  type t
  type space
  type basic_map
  type union_map
  type constrnt
    
  val empty : space -> t
  val universe : space -> t
  val nat_universe : space -> t
  val identity : space -> t
  val lex_lt : space -> t
  val lex_le : space -> t
  val lex_gt : space -> t
  val lex_ge : space -> t
  val lex_lt_first : space -> int -> t
  val lex_le_first : space -> int -> t
  val lex_gt_first : space -> int -> t
  val lex_ge_first : space -> int -> t
  val from_basic_map : basic_map -> t
  val from_union_map : union_map -> t
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

module type UNION_SET = sig
  type t
  type space
  type set
  
  val empty : space -> t
  val universe : space -> t
  val from_set : set -> t

  (* val n_set : t -> int *)
  (* val foreach_set : t -> (set -> unit) -> unit *)
  (* val extract_set : t -> space -> set *)
end

module type UNION_MAP = sig
  type t
  type space
  type map

  val empty : space -> t
  val universe : space -> t
  val from_map : map -> t

  (* val n_map : t -> int     *)
  (* val foreach_map : t -> (t -> unit) -> unit *)
  (* val extract_map : t -> space -> map *)

  (* val get_dim_id : t -> dim_type -> int -> id option *)
end

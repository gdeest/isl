open IslRaw
open Isl_Types    
open Isl_Utils
open Isl_Sigs

module Make (CTX: CTX): CONSTRAINT
  with type t = IslRaw.constrnt
   and type value = IslRaw.value
   and type local_space = IslRaw.local_space
   and type aff = IslRaw.aff
= struct
  open CTX
  type t = constrnt
  type value = IslRaw.value
  type local_space = IslRaw.local_space
  type aff = IslRaw.aff    

  let val_copy = IslRaw.Value.copy
  let ls_copy = IslRaw.LocalSpace.copy
  let copy = Constraint.copy
  let wrap = wrap Constraint.free

  let equality_alloc s = wrap @@ Constraint.equality (ls_copy s)
  let inequality_alloc s = wrap @@ Constraint.inequality (ls_copy s)
  let set_constant_si c i = wrap @@ Constraint.set_constant_si (copy c) i
  let set_constant_val c v = wrap @@ Constraint.set_constant_val (copy c) (val_copy v)
  let set_coefficient_si c dim_type pos i = wrap @@ Constraint.set_coefficient_si
      (copy c) (to_raw_dt dim_type) pos i
  let set_coefficient_val c dim_type pos v = wrap @@ Constraint.set_coefficient_val
      (copy c) (to_raw_dt dim_type) pos (val_copy v)

  let local_space c = Isl_Utils.wrap IslRaw.LocalSpace.free @@ Constraint.get_local_space c

  let is_lower_bound c dim_type pos = Constraint.is_lower_bound c (to_raw_dt dim_type) (Unsigned.UInt.of_int pos)
  let is_upper_bound c dim_type pos = Constraint.is_upper_bound c (to_raw_dt dim_type) (Unsigned.UInt.of_int pos)

  let get_constant_val c = Isl_Utils.wrap IslRaw.Value.free @@ Constraint.get_constant_val c
  let get_coefficient_val c dim_type pos = Isl_Utils.wrap IslRaw.Value.free @@ Constraint.get_coefficient_val c (to_raw_dt dim_type) pos
  let get_dim_name c dim_type pos = Constraint.get_dim_name c (to_raw_dt dim_type) (Unsigned.UInt.of_int pos)

  let get_type c = if Constraint.is_equality c then Equality else Inequality
  let get_div c pos = Isl_Utils.wrap IslRaw.Aff.free @@ Constraint.get_div c pos

  let involves_dims c dim_type ~first ~n = Constraint.involves_dims c (to_raw_dt dim_type) (Unsigned.UInt.of_int first) (Unsigned.UInt.of_int n)

end

open Isl_Sigs
open Isl_Types
open IslRaw

module Make (CTX: CTX): BASIC_SET
  with type t = IslRaw.basic_set
   and type space = IslRaw.space
   and type local_space = IslRaw.local_space
   and type matrix = IslRaw.matrix
   and type constrnt = IslRaw.constrnt
= struct
  open CTX
      
  type t = IslRaw.basic_set
  type space = IslRaw.space
  type local_space = IslRaw.local_space
  type matrix = IslRaw.matrix
  type constrnt = IslRaw.constrnt

  let copy = BasicSet.copy
  let wrap = Isl_Utils.wrap BasicSet.free

  let empty s = wrap @@ BasicSet.empty (Space.copy s)
  let universe s = wrap @@ BasicSet.universe (Space.copy s)
  let nat_universe s = wrap @@ BasicSet.nat_universe (Space.copy s)
  let add_constraint bs c = wrap @@ BasicSet.add_constraint
      (copy bs) (Constraint.copy c)
  let drop_constraint bs c = wrap @@ BasicSet.drop_constraint
      (copy bs) (Constraint.copy c)
  let local_space bs = Isl_Utils.wrap IslRaw.LocalSpace.free @@ BasicSet.get_local_space bs

  let remove_divs bs = wrap @@ BasicSet.remove_divs (copy bs)
  let remove_divs_involving_dims bs dim_type ~first ~n =
    wrap @@ BasicSet.remove_divs_involving_dims (copy bs) (to_raw_dt dim_type) (Unsigned.UInt.of_int first) (Unsigned.UInt.of_int n)
  let remove_unknown_divs bs = wrap @@ BasicSet.remove_unknown_divs (copy bs)

  let n_constraint = BasicSet.n_constraint
  let foreach_constraint bs f =
    let f c _ = if (f c) then 0 else (-1) in
    let _ = BasicSet.foreach_constraint bs f Ctypes.null in ()

  let equalities_matrix bs dt1 dt2 dt3 dt4 = Isl_Utils.wrap IslRaw_Matrix.free @@
    let tdt = to_raw_dt in 
    BasicSet.equalities_matrix bs (tdt dt1) (tdt dt2) (tdt dt3) (tdt dt4)

  let inequalities_matrix bs dt1 dt2 dt3 dt4 = Isl_Utils.wrap IslRaw_Matrix.free @@
    let tdt = to_raw_dt in 
    BasicSet.inequalities_matrix bs (tdt dt1) (tdt dt2) (tdt dt3) (tdt dt4)    

  let to_string = Isl_Utils.print_with ctx IslRaw.Printer.print_basic_set
end

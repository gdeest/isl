open Isl_Sigs

module Make (CTX: CTX): BASIC_MAP
  with type t = IslRaw.basic_map
   and type space = IslRaw.space
   and type local_space = IslRaw.local_space
   and type constrnt = IslRaw.constrnt
=
struct
  type t = IslRaw.basic_map
  type space = IslRaw.space
  type local_space = IslRaw.local_space
  type constrnt = IslRaw.constrnt

  module M = IslRaw.BasicMap
               
  let copy = M.copy
  let wrap = Isl_Utils.wrap M.free

  let empty s = wrap @@ M.empty (IslRaw.Space.copy s)
  let universe s = wrap @@ M.universe (IslRaw.Space.copy s)
  let identity s = wrap @@ M.identity (IslRaw.Space.copy s)
  let nat_universe s = wrap @@ M.nat_universe (IslRaw.Space.copy s)
  let add_constraint bm c = wrap @@ M.add_constraint
      (copy bm) (IslRaw.Constraint.copy c)

  let local_space bs = Isl_Utils.wrap IslRaw.LocalSpace.free @@ M.get_local_space bs

  let to_string = Isl_Utils.print_with CTX.ctx IslRaw.Printer.print_basic_map
end

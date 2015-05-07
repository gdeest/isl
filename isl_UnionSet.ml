open Isl_Sigs

module Make (CTX: CTX): UNION_SET
  with type t = IslRaw.union_set
   and type space = IslRaw.space
   and type set = IslRaw.set
=
struct
  type t = IslRaw.union_set
  type space = IslRaw.space
  type set = IslRaw.set
                    
  module M = IslRaw.UnionSet
               
  let copy = M.copy
  let wrap = Isl_Utils.wrap M.free

  let empty s = wrap @@ M.empty (IslRaw.Space.copy s)
  let universe s = wrap @@ M.universe (IslRaw.Space.copy s)
  let from_set s = wrap @@ M.from_set (IslRaw.Set.copy s)
end

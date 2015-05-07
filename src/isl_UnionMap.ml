open Isl_Sigs

module Make (CTX: CTX): UNION_MAP
  with type t = IslRaw.union_map
   and type space = IslRaw.space
   and type map = IslRaw.map
=
struct
  type t = IslRaw.union_map
  type space = IslRaw.space
  type map = IslRaw.map
                    
  module M = IslRaw.UnionMap
               
  let copy = M.copy
  let wrap = Isl_Utils.wrap M.free

  let empty s = wrap @@ M.empty (IslRaw.Space.copy s)
  let universe s = wrap @@ M.universe (IslRaw.Space.copy s)
  let from_map s = wrap @@ M.from_map (IslRaw.Map.copy s)
end

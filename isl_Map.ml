open Isl_Sigs

module Make (CTX: CTX): MAP
  with type t = IslRaw.map
   and type space = IslRaw.space
   and type basic_map = IslRaw.basic_map
   and type union_map = IslRaw.union_map
   and type constrnt = IslRaw.constrnt
=
struct
  type t = IslRaw.map
  type space = IslRaw.space
  type basic_map = IslRaw.basic_map
  type union_map = IslRaw.union_map
  type constrnt = IslRaw.constrnt  

  module M = IslRaw.Map

  let copy = M.copy
  let wrap = Isl_Utils.wrap M.free

  let empty s = wrap @@ M.empty (IslRaw.Space.copy s)
  let universe s = wrap @@ M.universe (IslRaw.Space.copy s)
  let nat_universe s = wrap @@ M.nat_universe (IslRaw.Space.copy s)
  let identity s = wrap @@ M.identity (IslRaw.Space.copy s)

  let lex_lt s = wrap @@ M.lex_lt (IslRaw.Space.copy s)
  let lex_le s = wrap @@ M.lex_le (IslRaw.Space.copy s)
  let lex_gt s = wrap @@ M.lex_gt (IslRaw.Space.copy s)
  let lex_ge s = wrap @@ M.lex_ge (IslRaw.Space.copy s)
  let lex_lt_first s i = wrap @@ M.lex_lt_first (IslRaw.Space.copy s) i
  let lex_le_first s i = wrap @@ M.lex_le_first (IslRaw.Space.copy s) i
  let lex_gt_first s i = wrap @@ M.lex_gt_first (IslRaw.Space.copy s) i
  let lex_ge_first s i = wrap @@ M.lex_ge_first (IslRaw.Space.copy s) i

  let from_basic_map bm = wrap @@ M.from_basic_map (IslRaw.BasicMap.copy bm)
  let from_union_map um = wrap @@ M.from_union_map (IslRaw.UnionMap.copy um)

  let add_constraint m c = wrap @@ M.add_constraint (copy m) (IslRaw.Constraint.copy c)

  let to_string = Isl_Utils.print_with CTX.ctx IslRaw.Printer.print_map
end

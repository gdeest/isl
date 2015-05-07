module ISL = Isl.Make ()

let _ =
  let open ISL in
  (* Create a space with 2 params and 3 dimensions *)
  let space = Space.set_alloc ~nparam:2 ~ndim:3 in
  let space = Space.set_dim_name space `Param 0 "M" in
  let space = Space.set_dim_name space `Param 1 "N" in
  let space = Space.set_dim_name space `Set 0 "x" in
  let space = Space.set_dim_name space `Set 1 "y" in
  let space = Space.set_dim_name space `Set 2 "z" in
  (* Create a "universe" basic set on this space. *)
  let bset = BasicSet.universe space in
  (* Prints: [M, N] -> { [x, y, z] } *)
  print_endline @@ BasicSet.to_string bset;
  (* Add constraint: 2M -7x + y -3 >= 0 to basic set *)
  let constr = Constraint.inequality_alloc (LocalSpace.from_space space) in
  let constr = Constraint.set_coefficient_si constr `Param 0 2 in
  let constr = Constraint.set_coefficient_si constr `Set 0 (-7) in
  let constr = Constraint.set_coefficient_si constr `Set 1 1 in
  let constr = Constraint.set_constant_si constr (-3) in
  let bset = BasicSet.add_constraint bset constr in
  (* Prints: [M, N] -> { [x, y, z] : y >= 3 - 2M + 7x } *)
  print_endline @@ BasicSet.to_string bset


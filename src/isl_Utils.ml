let wrap free v =
  Gc.finalise free v; v  

let print_with ctx f o =
  let open IslRaw.Printer in
  let printer = string_printer ctx in
  let printer = f printer o in
  let str = get_string printer in
  free printer; str

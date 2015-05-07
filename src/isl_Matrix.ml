open Isl_Sigs
open IslRaw
  
module Make (CTX: CTX): MATRIX
  with type t = matrix
= struct
  type t = matrix
end

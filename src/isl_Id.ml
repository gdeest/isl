open Isl_Utils
open Isl_Sigs
open IslRaw

module Make (CTX: CTX) : ID with type t = id = struct
  type t = id
  let make name = wrap Id.free @@ Id.alloc CTX.ctx name Ctypes.null
end

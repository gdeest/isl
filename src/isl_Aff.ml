open IslRaw
open Isl_Types    
open Isl_Utils
open Isl_Sigs

module Make (CTX: CTX): AFF
  with type t = IslRaw.aff
= struct
  open CTX
  type t = IslRaw.aff
end

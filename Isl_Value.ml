open Isl_Utils
open Isl_Sigs
open IslRaw

module Make (CTX: CTX) : VALUE with type t = value = struct
  type t = value

  open CTX
  open Value
  
  let wrap = wrap free
  let copy = copy

  let zero = wrap @@ zero ctx        
  let one = wrap @@ one ctx
  let negone = wrap @@ negone ctx
  let nan = wrap @@ nan ctx
  let infty = wrap @@ infty ctx
  let neginfty = wrap @@ neginfty ctx

  let num v = Signed.Long.to_int64 @@ get_num_si v
  let den v = Signed.Long.to_int64 @@ get_den_si v

  let sgn = sgn
  let is_zero = is_zero
  let is_one = is_one
  let is_negone = is_negone
  let is_nonneg = is_nonneg
  let is_nonpos = is_nonpos
  let is_pos = is_pos
  let is_neg = is_neg
  let is_int = is_int
  let is_rat = is_rat
  let is_nan = is_nan
  let is_infty = is_infty
  let is_neginfty = is_neginfty

  let lt = lt
  let le = le
  let gt = gt
  let ge = ge
  let eq = eq
  let ne = ne
  let divisible = is_divisible_by
  let cmp_int64 v i = cmp_si v (Signed.Long.of_int64 i)
  let (<), (<=), (>), (>=), (=), (<>) = lt, le, gt, ge, eq, ne        

  let abs v = wrap @@ abs @@ copy v
  let neg v = wrap @@ neg @@ copy v
  let floor v = wrap @@ floor @@ copy v
  let ceil v = wrap @@ ceil @@ copy v
  let trunc v = wrap @@ trunc @@ copy v
  let twoexp v = wrap @@ two_exp @@ copy v

  let min v1 v2 = wrap @@ min (copy v1) (copy v2)
  let max v1 v2 = wrap @@ max (copy v1) (copy v2)        
  let add v1 v2 = wrap @@ add (copy v1) (copy v2)
  let add_int v i = wrap @@ add_ui (copy v) (Unsigned.ULong.of_int i)
  let sub v1 v2 = wrap @@ sub (copy v1) (copy v2)
  let sub_int v i = wrap @@ sub_ui (copy v) (Unsigned.ULong.of_int i)
  let mul v1 v2 = wrap @@ mul (copy v1) (copy v2)
  let mul_int v i = wrap @@ mul_ui (copy v) (Unsigned.ULong.of_int i)
  let div v1 v2 = wrap @@ div (copy v1) (copy v2)

  let modulo a b = wrap @@ modulo (copy a) (copy b)
  let gcd a b = wrap @@ gcd (copy a) (copy b)
  let gcdext a b =
    let open Ctypes in
    let x_ptr = allocate_n value ~count:1 in
    let y_ptr = allocate_n value ~count:1 in
    let d = wrap @@ gcdext a b x_ptr y_ptr in
    (d, wrap !@x_ptr, wrap !@y_ptr)

  let (+), (-), (~-), ( * ), (mod), (/) = add, sub, neg, mul, modulo, div

  let of_int64 i =
    let i = Signed.Long.of_int64 i in
    wrap @@ int_from_si ctx i

  let of_int i = of_int64 @@ Int64.of_int i
  let to_int64 v = if (is_int v) then Some (num v) else None    
end

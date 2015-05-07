type dim_type = [ `Param | `In | `Out | `Set ]
type noparam = [ `In | `Out | `Set ]

let to_raw_dt = function
  | `Param -> IslRaw_Types.DimParam
  | `In -> IslRaw_Types.DimIn
  | `Out -> IslRaw_Types.DimOut
  | `Set -> IslRaw_Types.DimSet                        

type constrnt_type = Equality | Inequality                       

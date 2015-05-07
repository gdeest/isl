type dim_type = [ `Param | `In | `Out | `Set ]
type noparam = [ `In | `Out | `Set ]

type constrnt_type = Equality | Inequality                       

val to_raw_dt : [< dim_type] -> IslRaw_Types.dim_type

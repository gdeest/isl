open Ctypes

type ctx
val ctx : ctx typ
    
type value
val value : value typ
    
type id
val id : id typ
    
type space
val space : space typ
    
type local_space
val local_space : local_space typ
    
type dim_type = DimCst | DimParam | DimIn | DimOut | DimSet | DimDiv | DimAll
val dim_type : dim_type typ
    
type matrix
val matrix : matrix typ
    
type aff
val aff : aff typ
    
type pw_aff
val pw_aff : pw_aff typ
    
type multi_aff
val multi_aff : multi_aff typ
    
type pw_multi_aff
val pw_multi_aff : pw_multi_aff typ
    
type constrnt
val constrnt : constrnt typ
    
type basic_set
val basic_set : basic_set typ
    
type basic_map
val basic_map : basic_map typ
    
type set
val set : set typ
    
type map
val map : map typ
    
type union_set
val union_set : union_set typ
    
type union_map
val union_map : union_map typ
    
type printer
val printer : printer typ


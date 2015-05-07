open Ctypes
    
type ctx = unit ptr
let ctx : ctx typ = ptr void

type value = unit ptr
let value : value typ = ptr void

type id = unit ptr
let id : id typ = ptr void

type space = unit ptr
let space : space typ = ptr void

type local_space = unit ptr
let local_space : local_space typ = ptr void

type dim_type = DimCst | DimParam | DimIn | DimOut | DimSet | DimDiv | DimAll

type matrix = unit ptr
let matrix : matrix typ = ptr void

type aff = unit ptr
let aff : aff typ = ptr void

type pw_aff = unit ptr
let pw_aff : pw_aff typ = ptr void

type multi_aff = unit ptr
let multi_aff : multi_aff typ = ptr void

type pw_multi_aff = unit ptr
let pw_multi_aff : pw_multi_aff typ = ptr void

type constrnt = unit ptr
let constrnt : constrnt typ = ptr void

type basic_set = unit ptr
let basic_set : basic_set typ = ptr void

type basic_map = unit ptr
let basic_map : basic_map typ = ptr void

type set = unit ptr
let set : set typ = ptr void

type map = unit ptr
let map : map typ = ptr void

type union_set = unit ptr
let union_set : union_set typ = ptr void

type union_map = unit ptr
let union_map : union_map typ = ptr void

type printer = unit ptr
let printer : printer typ = ptr void

let int_of_dim_type = function
  | DimCst -> 0
  | DimParam -> 1
  | DimIn -> 2
  | DimOut | DimSet -> 3
  | DimDiv -> 4
  | DimAll -> 5

let dim_type_of_int = function
  | 0 -> DimCst
  | 1 -> DimParam
  | 2 -> DimIn
  | 3 -> DimOut
  | 4 -> DimDiv
  | 5 -> DimAll
  | _ -> raise @@ Invalid_argument "Expecting int in range 0-5."
                    
let dim_type : dim_type typ = view ~read:dim_type_of_int ~write:int_of_dim_type int




    

open Ctypes
open Foreign
open IslRaw_Types
    
let string_printer = foreign "isl_printer_to_str" (ctx @-> returning printer)
let free = foreign "isl_printer_free" (printer @-> returning void)
let get_string = foreign "isl_printer_get_str" (printer @-> returning string)

let print_space = foreign "isl_printer_print_space" (printer @-> space @-> returning printer)
let print_basic_set = foreign "isl_printer_print_basic_set" (printer @-> basic_set @-> returning printer)
let print_basic_map = foreign "isl_printer_print_basic_map" (printer @-> basic_map @-> returning printer)
    
let print_set = foreign "isl_printer_print_set" (printer @-> set @-> returning printer)
let print_map = foreign "isl_printer_print_map" (printer @-> map @-> returning printer)        

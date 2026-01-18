open! Core

(* Arg parse values *)
let usage_msg = "Usage: input_size <input file>"
let file = ref ""
let anon_fun fn = file := fn

(* Function that parses a line and returns the max value,
   meant to be used in List.fold *)
let max_val acc s =
  let n = int_of_string ( String.slice s 1 (String.length s) ) in
  (* print_endline (string_of_int n); *)
  if n > acc then n else acc;;

(* Main *)
let () =
  Arg.parse [] anon_fun usage_msg;
  print_string "Input file: ";
  if not (String.equal !file "") then 
    print_endline !file 
  else 
    (print_endline usage_msg; exit 1);

  (* Read file + calculate *)
  let lines = In_channel.read_lines !file in
  let max = List.fold ~init:0 ~f:max_val lines in
  let min_bitwidth = Int.ceil_log2 max in
  
  (* Output *)
  print_endline ("Max: " ^ string_of_int max);
  print_endline (
    "Min. input bit width: " ^ string_of_int min_bitwidth ^
    " (max representable: " ^ string_of_int (1 lsl min_bitwidth) ^ ")"
  )
;;

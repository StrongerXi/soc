open Pervasives

let () =
  let filename = Sys.argv.(1) in
  match Driver.lir_file filename with
  | Ok lir_prog ->
    let asm_prog = Driver.lir_to_arm64 lir_prog in
    let asm_prog_str = Arm64.func_prog_to_str asm_prog in
    let extless_filename, _ = Filename.split_extension filename in
    let output_filename = extless_filename ^ ".s" in
    Io.write_file output_filename asm_prog_str

  | Error msg -> Io.println msg
;;

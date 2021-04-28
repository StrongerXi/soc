open Pervasives

let () =
  let filename = Sys.argv.(1) in
  match Driver.lir_file filename with
  | Ok lir_prog ->
    let x86_prog = Driver.lir_to_x86 lir_prog in
    let x86_prog_str = X86.func_prog_to_str x86_prog in
    let extless_filename, _ = Filename.split_extension filename in
    let output_filename = String.append extless_filename ".s" in
    Io.write_file output_filename x86_prog_str

  | Error msg -> Io.println msg
;;

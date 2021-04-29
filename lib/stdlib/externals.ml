
(** - What?
      All functions that require external implementation
    
    - Why?
      (1). In the OCaml Stdlib, they are (directly or indirectly) implemented
      with the `external` keyword. This isn't hard to implement in SOC, but I
      don't really want to dig up how to configure dune to build the project
      with runtime code. So I'm using the existing functions in OCaml's Stdlib.
      
      (2). To self-compile SOC with SOC, I just need to swap this file with
      another one that uses the `external` keyword, and link against my runtime
      code. Single point of control:).
    
    NOTE 
    - Don't use this module outside the stdlib directory, so that one day we can
      easily swap it out.
    - This module should depend on no other modules in the compiler codebase.
      Think of it as sitting on the outermost edge of the compiler.
    *)

type 'a list = 
  | []
  | (::) of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e


let read_entire_file (filename : string) : string =
  let ch = Stdlib.open_in filename in
  let s = Stdlib.really_input_string ch (Stdlib.in_channel_length ch) in
  Stdlib.close_in ch;
  s
;;

let write_entire_file (filename : string) (content : string) : unit =
  let oc = Stdlib.open_out filename in
  Stdlib.output_string oc content;
  Stdlib.close_out oc;
;;

let print s =
  Stdlib.print_string s;
  Stdlib.flush Stdlib.stdout
;;

let char_code = Stdlib.Char.code
;;

let char_to_string = Stdlib.Char.escaped
;;

let int_to_string = Stdlib.string_of_int
;;

let int_of_string_opt s = 
  match Stdlib.int_of_string_opt s with
  | Some n -> Some n
  | None -> None
;;

let string_length = Stdlib.String.length
;;

let string_get = Stdlib.String.get
;;

let string_sub = Stdlib.String.sub
;;

let string_append = Stdlib.(^)
;;

let raise = Stdlib.raise
;;

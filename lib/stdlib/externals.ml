
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
    
    NOTE Don't use it outside the stdlib directory, so that one day we can
    easily swap it out.
    *)

let read_entire_file (filename : string) : string =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let write_entire_file (filename : string) (content : string) : unit =
  let oc = open_out filename in
  output_string oc content;
  close_out oc;
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

let int_of_string = Stdlib.int_of_string
;;

let string_length = Stdlib.String.length
;;

let string_get = Stdlib.String.get
;;

let string_sub = Stdlib.String.sub
;;

let string_append = (^)
;;

let failwith = failwith
;;

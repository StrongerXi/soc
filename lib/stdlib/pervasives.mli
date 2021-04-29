
(* The OCaml compiler loads the pervasives module by default.
   Centralizing the "auto-loaded" functions here makes self-compilation easier.
   For now we'll just manually open it in each file, if needed. *)

(* NOTE these type definitions are technically redundant, but they help enforce
 * the separation between our custom stdlib and OCaml's built-in stdlib. 
 * TODO they will be completely removed once we add them as pre-defined types in
 * the compiler. *)
type 'a list         = 'a Externals.list =
  | []
  | (::) of 'a * 'a list

type 'a option       = 'a Externals.option =
  | None
  | Some of 'a

type ('a, 'e) result = ('a, 'e) Externals.result =
  | Ok of 'a
  | Error of 'e

val not : bool -> bool

(** [int_of_string_opt s] returns either an integer which [s] represents, or
    [None] if [s] isn't a valid int *)
val int_of_string_opt : string -> int option

(** Pipelining *)
val (|>) : 'a -> ('a -> 'b) -> 'b

(** Application *)
val (@@) : ('a -> 'b) -> 'a -> 'b

(** [xs @ ys] is a list with elements of [xs] added to the front of [ys] in
    order *)
val (@) : 'a list -> 'a list -> 'a list

(** [s1 ^ s2] is a string with [s1] and [s2] appended together *)
val (^) : string -> string -> string

(** [raise exn] will transfer control flow to the dynamically enclosing
    try-with block that expects [exn], or print out the error at top-level. *)
val raise : exn -> 'a

(** [failwith msg] = [raise (Failure msg)] *)
val failwith : string -> 'a

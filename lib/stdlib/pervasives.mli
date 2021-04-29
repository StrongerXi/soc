
(* The OCaml compiler loads the pervasives module by default.
   Centralizing the "auto-loaded" functions here makes self-compilation easier.
   For now we'll just manually open it in each file, if needed. TODO *)


(** NOTE
    These types are built-in to the OCaml compiler.
    I decided to declare them explicitly for now; to make them built-in, simply
    add them into the initial environment and remove these definitions *)

type 'a list = 
  | []
  | (::) of 'a * 'a list

type 'a option =
  | None
  | Some of 'a

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

val not : bool -> bool

(** Pipelining *)
val (|>) : 'a -> ('a -> 'b) -> 'b

(** Application *)
val (@@) : ('a -> 'b) -> 'a -> 'b

(** [xs @ ys] is a list with elements of [xs] added to the front of [ys] in
    order *)
val (@)  : 'a list -> 'a list -> 'a list

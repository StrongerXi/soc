
(** A directed graph parameterized over attribute associated with each node *)
type 'a t

(** A [node] is an opaque handle to internal node in some ['a t].
    NOTE Error if any input node is removed or not created within input [t] *)
type node

(** t is immutable, but OCaml's type system disallows generalization here... *)
val empty_graph : unit -> 'a t

(** [create_node t annot] create a unique node in [t] annotated with [annot]
    The annotation type can change later, as long as the node is still in [t] *)
val create_node : 'a t -> 'a -> ('a t * node)

(** [remove_node t node] returns a new [t] with [node] completely removed, i.e.,
    as itself or neighbors of other nodes. *)
val remove_node : 'a t -> node -> 'a t

(* Some query functions *)
val get_annot : 'a t -> node -> 'a
val set_annot : 'a t -> node -> 'a -> 'a t
val add_edge  : 'a t -> node -> node -> 'a t
val get_out_neighbors : 'a t -> node -> node Set.t
val get_in_neighbors  : 'a t -> node -> node Set.t

(** in no particular order *)
val get_all_nodes     : 'a t -> node Set.t

(** All nodes created in [t] and are still present can still be used with output
    [t], despite potential change to the annotations *)
val map : ('a -> 'b) -> 'a t -> 'b t

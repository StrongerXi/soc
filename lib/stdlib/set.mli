open Pervasives

(** A ['e t] represents an immutable set of values with type ['e] 
    NOTE undefined behavior to use two ['e t] with different underlying
    comparison functions (passed in from [empty]). *)
type 'e t

(** [empty cmp] creates an empty set assuming [cmp x y] returns
    - negative if [x] is considered less than [y]
    - 0        if [x] is considered equal to [y]
    - positive if [x] is considered greater than [y]
    in a consistent manner *)
val empty : ('e -> 'e -> int) -> 'e t

(** [is_empty t] returns true iff [t] contains no elements *)
val is_empty : 'e t -> bool

(** [size t] returns the # of elements in [t] *)
val size : 'e t -> int

(** [add e t] returns a new set that contains [e] as well *)
val add : 'e -> 'e t -> 'e t

(** [remove e t] returns a set where [e] is absent *)
val remove : 'e -> 'e t -> 'e t

(** [get_one t] tries to get some element from [t].
    Return [None] if [t] is empty. *)
val get_one : 'e t -> 'e option

(** [mem e t] returns true iff [e] is present in [t] *)
val mem : 'e -> 'e t -> bool

(** [map f t] returns a new map where each value [v] in [t] becomes [f v] *)
val map : ('e -> 'e) -> 'e t -> 'e t

(** [filter p t] returns a subset of [t] whose elements satisfy [p]. *)
val filter : ('e -> bool) -> 'e t -> 'e t

(** [find p t] returns some element in [t] that satisfies [p],
    or None if such element doesn't exist in [t]. *)
val find : ('e -> bool) -> 'e t -> 'e option

(** [fold f a t] is (f (... (f (f a e1) e2)  ...) en),
    where e1 ... eN are all the elements in [t] *)
val fold : ('a -> 'e -> 'a) -> 'a -> 'e t -> 'a

(** [union t1 t2] returns a new set that contains all the elements in either 
    [t1] or [t2]. *)
val union : 'e t -> 'e t -> 'e t

(** [inter t1 t2] returns a new set that contains the common elements in [t1]
    and [t2]. *)
val inter : 'e t -> 'e t -> 'e t

(** [disjoint t1 t2] returns true iff [t1] and [t2] contain no common element *)
val disjoint : 'e t -> 'e t -> bool

(** [diff s1 s2] contains the elements of [s1] that are not in [s2]. *)
val diff : 'e t -> 'e t -> 'e t

(** [subset s1 s2] returns true iff all elements in [s1] are in [s2] as well. *)
val subset : 'e t -> 'e t -> bool

(** [to_list t] returns all elements in the set in no particular order *)
val to_list : 'e t -> 'e list

(** [to_string f t] returns a string representation of the [t], using [f] to
    format individual elements *)
val to_string : ('e -> string) -> 'e t -> string

(** [get_compare_func t] returns the compare function for elements of [t],
    passed in during [t]'s construction. *)
val get_compare_func : 'e t -> ('e -> 'e -> int)

open Pervasives

(** [cons x xs] is [x::xs] *)
val cons : 'a -> 'a list -> 'a list

(** [length xs] returns # of elements in [xs] *)
val length : 'a list -> int

(** [rev xs] returns a reversed version of [xs] *)
val rev : 'a list -> 'a list

(** [append xs ys] returns a list with elements of [xs] added to [ys] in order *)
val append : 'a list -> 'a list -> 'a list

(** [flatten xs] returns a list with the inner lists appended in order *)
val concat : 'a list list -> 'a list

(** alias for [concat] *)
val flatten : 'a list list -> 'a list

(** [map f [a1; ...; an]] is [f a1; ...; f an] *)
val map : ('a -> 'b) -> 'a list -> 'b list

(** [fold_left f a [b1; ...; bn]] is f (... (f (f a b1) b2) ...) bn]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** [fold_right f [a1; ...; an] b] is [f a1 (f a2 (... (f an b) ...))] *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(** [for_all p [a1; ...; an]] is [(p a1) && (p a2) && ... && (p an)],
    or true if list is empty *)
val for_all : ('a -> bool) -> 'a list -> bool

(** [exists p [a1; ...; an]] is [(p a1) || (p a2) || ... || (p an)]
    or false if list is empty *)
val exists : ('a -> bool) -> 'a list -> bool

(** [mem a xs] is true if and only if [a = x] for some element [x] in [xs] *)
val mem : 'a -> 'a list -> bool

(** [find_opt p xs] returns the first element of the list xs that satisfies the
 * predicate p, or None if there is no value that satisfies p in the list xs. *)
val find_opt : ('a -> bool) -> 'a list -> 'a option

(** [filter p xs] returns all the elements [x] in [xs] s.t. [p x = true].
    The order of the elements in the input list is preserved. *)
val filter : ('a -> bool) -> 'a list -> 'a list

(** [partition p xs] returns [(l1, l2)], where
    x ∈ [l1] => [p x = true] and x ∈ [l2] => [p x = false].
    The order of the elements in the input list is preserved. *)
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

(** [assoc_opt a xs] returns the first value associated with key [a] in [xs]
    pairs xs. Returns None if such a value doesn't exist *)
val assoc_opt : 'a -> ('a * 'b) list -> 'b option

(** [remove_assoc a xs] returns the list of pairs [xs] without the first pair
    with key [a], if any. *)
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list

(** [remove_dups eq xs] returns a new list with duplicated elements (based on
    [eq]) removed; the first-encountered elements are preserved, so is the
    relative order among elements. *)
val remove_dups : ('a -> 'a -> bool) -> 'a list -> 'a list

(** [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])]. *)
val split : ('a * 'b) list -> 'a list * 'b list

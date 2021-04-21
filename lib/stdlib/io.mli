

(** [read_file path] returns the entire content of the file located at [path];
    errors on any IO issue *)
val read_file : string -> string

(** [write_file path content] creates or overwrites a file at [path] so that
    it contains [content]; errors on any IO issue *)
val write_file : string -> string -> unit

(** [println s] prints [s] and a newline to stdout *)
val println : string -> unit

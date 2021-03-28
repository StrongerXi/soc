

(** [read_file path] returns the entire content of the file located at [path];
    errors on any IO issue *)
val read_file : string -> string

(** [println s] prints [s] and a newline to stdout *)
val println : string -> unit

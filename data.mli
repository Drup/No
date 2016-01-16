(** The database containing the words. *)

(** Returns a sequence of languages where bindings in l are
    on the front and the rest follows, in a non-determined order. *)
val distinguish :
  (string, 'a) Hashtbl.t -> string list -> 'a Sequence.t

val no : (string, string) Hashtbl.t

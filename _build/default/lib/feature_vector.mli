open Types

(** Feature Vector Indexing for fast subsumption filtering. *)

type t

(** Create a new empty index. *)
val create : unit -> t

(** Add a clause to the index. *)
val add : t -> clause -> int -> unit

(** Remove a clause from the index. *)
val remove : t -> int -> unit

(** Find candidate clauses that might be subsumed by the given clause (Backward Subsumption).
    Returns a list of clause IDs. *)
val find_subsumed_candidates : t -> clause -> int list

(** Find candidate clauses that might subsume the given clause (Forward Subsumption).
    Returns a list of clause IDs. *)
val find_subsuming_candidates : t -> clause -> int list

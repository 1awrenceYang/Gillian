open Gil_syntax

exception SMT_unknown
exception SMT_error of string

val exec_sat : Expr.Set.t -> (string, Type.t) Hashtbl.t -> Sexplib.Sexp.t option
val is_sat : Expr.Set.t -> (string, Type.t) Hashtbl.t -> bool

val check_sat :
  Expr.Set.t -> (string, Type.t) Hashtbl.t -> Sexplib.Sexp.t option

val lift_model :
  Sexplib.Sexp.t ->
  (string, Type.t) Hashtbl.t ->
  (string -> Expr.t -> unit) ->
  Expr.Set.t ->
  unit

val pp_sexp : Sexplib.Sexp.t Fmt.t

(** Current per-query solver timeout in milliseconds ([None] if the value taken
    from the [SMT_TIMEOUT] environment variable is not an integer). *)
val timeout_ms : unit -> int option

(** Change the per-query solver timeout (milliseconds). Applies to the running
    solver immediately and to any restarted solver. *)
val set_timeout_ms : int -> unit

(** Version string reported by the solver ([(get-info :version)]). *)
val solver_version : unit -> string

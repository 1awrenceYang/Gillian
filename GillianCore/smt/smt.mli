open Gil_syntax

exception SMT_unknown
exception SMT_error of string

(** {2 SERVPIPS} *)

(** SERVPIPS mode (set by [Servpips.enable]). When set, a query answered
    [unknown] makes {!check_sat}/{!exec_sat} return [Some unknown_model]
    instead of raising, and any exception raised while encoding a query is
    re-raised as {!SMT_encoding_failure}. *)
val servpips_mode : bool ref

(** Number of queries actually sent to the solver (cache hits excluded). *)
val query_count : int ref

exception SMT_encoding_failure of string

(** The pseudo-model returned for an [unknown] answer in SERVPIPS mode. It is
    not a real model: [lift_model] must not be called on it. *)
val unknown_model : Sexplib.Sexp.t

val is_unknown_model : Sexplib.Sexp.t -> bool

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

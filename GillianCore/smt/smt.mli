open Gil_syntax

(** SERVPIPS builtin functions (see {!Servpips_functions}). *)
module Servpips_functions = Servpips_functions

exception SMT_unknown
exception SMT_error of string

(** {2 SERVPIPS} *)

(** SERVPIPS mode (set by [Servpips.enable]). When set, a query answered
    [unknown] makes {!check_sat}/{!exec_sat} return [Some unknown_model]
    instead of raising, and any exception raised while encoding a query is
    re-raised as {!SMT_encoding_failure}. *)
val servpips_mode : bool ref

(** Turn SERVPIPS mode on (idempotent): sets {!servpips_mode} and sends
    [(set-option :encoding bmp)] (strings are sequences of UTF-16 code
    units; also re-sent when the solver is restarted). In SERVPIPS mode the
    encodings of [ToStringOp], [ToNumberOp], [ToUint32Op], [ToInt32Op],
    [ToUint16Op], [FMod], [M_floor], [M_ceil], [M_abs], [M_sgn] and [M_round]
    are the SERVPIPS ones (design 4.4, 4.5, E5, E6), and non-finite number
    literals are an encoding failure (E7). Builtin functions
    ({!Servpips_functions}) are encoded in both modes. *)
val servpips_enable : unit -> unit

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

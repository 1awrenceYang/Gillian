(** @canonical Gillian.General.Servpips

    SERVPIPS mode: structured JSONL output of symbolic execution for the
    SERVPIPS validator (interface I2-OCaml of the SERVPIPS design, section 5.2;
    event format I1, section 5.1).

    The mode is off unless [gillian-js wpst --servpips ...] enabled it (see
    {!enable}); while it is off, {!emit} and {!note} do nothing and Gillian
    behaves exactly as upstream.

    Output: one JSON object per line, written to [(config ()).log_path] and
    flushed after every line, so the file stays parseable line by line even if
    the process is killed. Every line can be parsed by JavaScript's
    [JSON.parse]: non-finite floats anywhere in an emitted object are encoded
    as [{"nonfinite":"NaN"|"Infinity"|"-Infinity"}] (so a NaN literal in a GIL
    expression becomes [["Lit",["Num",{"nonfinite":"NaN"}]]]). *)

(** {2 Configuration} *)

type config = {
  log_path : string;
      (** [--servpips-log FILE]; default {!default_log_path}. The file is
          truncated when the mode is enabled. *)
  runtime_dir : string option;
      (** [--servpips-runtime DIR]: directory holding the [preamble.js] used
          for CommonJS programs (overrides [GILLIAN_JS_RUNTIME_PATH] for the
          preamble lookup). *)
  shard_json : Yojson.Safe.t;
      (** [--servpips-shard JSON], parsed; [`Null] when absent. Echoed in the
          [hello] event. *)
  smt_timeout_ms : int;
      (** Effective per-query SMT timeout in ms ([--smt-timeout], else the
          [SMT_TIMEOUT] environment variable, else Gillian's default 30000). *)
}

val default_log_path : string

(** Is SERVPIPS mode on? *)
val enabled : unit -> bool

(** Current configuration (meaningful only when {!enabled}). *)
val config : unit -> config

(** Turn SERVPIPS mode on with the given configuration and (re)open the log
    file (truncating it). Called by the [wpst] command line. *)
val enable : config -> unit

(** {2 Output} *)

(** Write one JSON object as one line of the log and flush. No-op unless
    {!enabled}. Non-finite floats are encoded as described above. Not
    thread-safe. *)
val emit : Yojson.Safe.t -> unit

(** [{"pp": <Expr.pp text>, "ast": <ppx_deriving_yojson Expr.t>}] with the
    non-finite number encoding. *)
val expr_json : Expr.t -> Yojson.Safe.t

(** A path condition: [[E, ...]], a conjunction of pure formulas. *)
val pc_json : Expr.t list -> Yojson.Safe.t

(** A type environment: [[[E, "Str"], ...]] ({!Type.str} names). *)
val types_json : (Expr.t * Type.t) list -> Yojson.Safe.t

(** Raised by a SERVPIPS extern (or any other engine hook) to end the current
    configuration: the interpreter emits
    [{"ev":"end","status":status,"reason":reason,...}] for it and drops the
    configuration (it is neither a successor nor an error result). Sibling
    configurations continue. *)
exception Path_end of { status : string; reason : string }

(** [{"ev":"note","code":code,"msg":msg,"site":site|null,"data":data|null}] *)
val note :
  code:string -> msg:string -> ?site:string -> ?data:Yojson.Safe.t -> unit -> unit

(** {2 Helpers used by the engine}

    Not part of the frozen I2-OCaml interface; they may be extended by the
    engine-core package. *)

(** The fork commit reported in [hello] (environment variable
    [SERVPIPS_FORK_COMMIT], set in the engine image; ["unknown"] otherwise). *)
val fork_commit : unit -> string

(** Extra [hello] field [builtins] (the SMT-LIB text of builtin definitions):
    by default [Servpips_functions.hello_json], i.e.
    [{"str.in_re.numlit": "<regex text>"}]. *)
val hello_builtins : (unit -> Yojson.Safe.t) ref

(** Emit the [hello] event:
    [{"ev":"hello","v":2,"fork":..,"z3":..,"shard":..,"unroll":..,
      "smt_timeout_ms":..,"builtins":..}] *)
val hello : unroll:int -> unit -> unit

(** Emit an [end] event:
    [{"ev":"end","status":..,"reason":..,"outcome":..|null,"pc":..,"types":..}] *)
val emit_end :
  status:string ->
  reason:string ->
  ?outcome:string ->
  pc:Expr.t list ->
  types:(Expr.t * Type.t) list ->
  unit ->
  unit

(** Split an assertion (as returned by [State.to_assertions]) into its pure
    formulas and its type environment. *)
val pc_and_types_of_asrt : Asrt.t -> Expr.t list * (Expr.t * Type.t) list

(** {2 Path accounting (engine core)}

    Every configuration that stops is a {e leaf}. A leaf is either reported by
    exactly one [end] event, or counted as [infeasible] (its path condition is
    unsatisfiable). Hence, in the final [stats] event,
    [leaves = Σ ends + infeasible], and [ends.truncated >= max_branch].

    The functions below are used by the interpreter ([g_interpreter.ml]) and
    by the solver interface; extern handlers normally only need
    {!Path_end}, {!end_path} and {!Assume_failed}. *)

(** The [end] statuses of the I1 format:
    [returned|threw|truncated|unknown|unsupported|error]. *)
val end_statuses : string list

(** Emit an [end] event and count the leaf. A status outside
    {!end_statuses} is reported as [error] (with the bad status in the
    reason). Long reasons are truncated. *)
val record_end :
  status:string ->
  reason:string ->
  ?outcome:string ->
  pc:Expr.t list ->
  types:(Expr.t * Type.t) list ->
  unit ->
  unit

(** [end{truncated, "max_branching"}] and [max_branch += 1] (E9). *)
val record_truncated :
  pc:Expr.t list -> types:(Expr.t * Type.t) list -> unit -> unit

(** Count a leaf whose path condition is unsatisfiable (no [end] event). *)
val record_infeasible : unit -> unit

(** A configuration without successors whose path condition is satisfiable
    (E13): [note{vanished, detail}] then [end{error, "vanished"}];
    [vanished += 1]. *)
val record_vanished :
  detail:string -> pc:Expr.t list -> types:(Expr.t * Type.t) list -> unit -> unit

(** A one-line description of an exception (analysis failures print their
    messages only). *)
val exn_msg : exn -> string

(** [note{internal-exception}] (the configuration is ended separately). *)
val internal_exception : msg:string -> unit

(** Raised by an extern (or any engine hook) that tried to assume a formula
    and found it unsatisfiable, e.g. [__servpips_assume(b)] with [b] false in
    the current state. Instead of returning [[]] (which the interpreter would
    audit as a vanished configuration against the path condition alone), the
    handler raises [Assume_failed f]: the interpreter re-checks
    [pc /\ f]; unsatisfiable → the leaf is counted as [infeasible],
    satisfiable → [end{error, "vanished"}] (the assumption was dropped although
    it is consistent). *)
exception Assume_failed of Expr.t

(** Like {!Path_end}, with an [outcome] for the [end] event (one of
    [resolved|rejected|pending|init-threw|no-handler|threw-sync], or [None]
    for [null]). [Path_end {status; reason}] is equivalent to
    [Path_end_outcome {status; reason; outcome = None}]. *)
exception Path_end_outcome of {
  status : string;
  reason : string;
  outcome : string option;
}

(** [end_path ~status ~reason ?outcome ()] raises {!Path_end_outcome}. *)
val end_path : status:string -> reason:string -> ?outcome:string -> unit -> 'a

(** Which engine component took the last negative decision (a formula
    decided false/unsatisfiable): ["reduction"], ["typing"] or ["solver"].
    Set by the symbolic state; read by the interpreter for [prune.by]. *)
val last_decision : string ref

val set_decision : string -> unit

(** Emit a [prune] event (E13) unless an identical one was already emitted:
    [{"ev":"prune","guard":E,"guard_orig":E,"kept":"then"|"else","by":..,
      "pc":[E..],"types":T}]. [guard] is the evaluated (reduced) guard,
    [guard_orig] the guard with the store substituted but not reduced; the
    dropped side is [not guard_orig] when [kept = "then"] and [guard_orig]
    when [kept = "else"].

    The emitted [pc]/[types] are the {!pc_slice} of the given ones for the
    variables of [guard] and [guard_orig]: a sub-conjunction of the path
    condition. Certifying [W(pc_slice) /\ W(dropped side)] unsatisfiable is
    sufficient (it implies the same for the full path condition), and slicing
    lets identical decisions made under different path conditions be
    deduplicated. The caller passes [pc = []] for decisions that do not
    depend on any context (the guard reduces to a literal on its own). *)
(** [pc_slice ~vars pc types]: the conjuncts of [pc] connected to [vars]
    (transitively, through shared logical variables and abstract locations),
    plus every conjunct without variables; and the entries of [types] for
    the connected variables. *)
val pc_slice :
  vars:Containers.SS.t ->
  Expr.t list ->
  (Expr.t * Type.t) list ->
  Expr.t list * (Expr.t * Type.t) list

val record_prune :
  guard:Expr.t ->
  guard_orig:Expr.t ->
  kept:string ->
  by:string ->
  pc:Expr.t list ->
  types:(Expr.t * Type.t) list ->
  unit ->
  unit

(** A solver query answered [unknown] was treated as satisfiable (sat query)
    or as not entailed (entailment query): emit
    [note{unknown-assumed-sat}] or [note{entail-unknown}] and count it (E3). *)
val note_unknown : entailment:bool -> unit

(** An SMT encoding failure: count it and raise
    [Path_end {status = "unsupported"; reason = "smt-encoding: " ^ msg}]. *)
val encode_failure : msg:string -> 'a

(** Record a fatal error for [stats.fatal] (first one wins). *)
val set_fatal : string -> unit

(** The recorded fatal error, if any. *)
val fatal : unit -> string option

(** Emit the final [stats] event (once):
    [{"ev":"stats","leaves":..,"ends":{..},"infeasible":..,"vanished":..,
      "prunes":..,"max_branch":..,"solver":{"queries":..,
      "unknown_assumed_sat":..,"entail_unknown":..,"encode_failures":..},
      "fatal":null|"..","seconds":..,"rss_mb":..}] *)
val emit_stats : unit -> unit

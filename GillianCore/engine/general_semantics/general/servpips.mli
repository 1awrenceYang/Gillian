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

(** Extra [hello] field [builtins] (the SMT-LIB text of builtin definitions,
    e.g. the [str.in_re.numlit] regex). Default: [`Assoc []]. *)
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

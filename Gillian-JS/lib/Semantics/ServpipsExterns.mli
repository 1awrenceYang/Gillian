(** Registry of SERVPIPS externs.

    {2 How a SERVPIPS extern is reached}

    The JS compiler turns every call [__servpips_<name>(a1, ..., an)] whose
    callee is that bare identifier (in any module of the program: the entry
    file or any [require]d file) into the JSIL external call
    {[
      x := extern "servpips_<name>"(v1, ..., vn) with err
    ]}
    where [v1 .. vn] are the argument {e values} (the arguments are evaluated
    left to right as JS expressions and GetValue is applied). [External.ml]
    forwards every extern whose name starts with ["servpips_"] to this
    registry. The value of the JS call expression is the value returned by the
    handler.

    If no handler is registered for the name, the configuration ends with
    [Servpips.Path_end {status = "unsupported"; reason = "unregistered servpips
    extern <name>"}].

    {2 Handler calling convention}

    A handler runs once per configuration reaching the call. It receives

    - a first-class module [E : ENV] giving access to the value and state
      modules of the running interpreter (concrete for [gillian-js exec],
      symbolic for [wpst]) and to the call site;
    - the current state;
    - the argument values;

    and returns the list of successor branches. Each branch is either
    [Return (state', v)]: continue after the call with [v] as the value of the
    call expression; or [Throw (state', v)]: jump to the call's JS error
    handling with [v] as the thrown value (i.e. as if the call threw [v]).
    Returning several branches forks the configuration; the handler is
    responsible for adding the branch conditions to each state (e.g. with
    [E.State.assume]) and for only returning satisfiable branches. Returning
    [[]] silently drops the configuration and must not be used to end a path:
    raise [Servpips.Path_end] instead, which ends the configuration with an
    [end] event (status/reason as given; the path condition reported is the
    one before the call). Handlers may also {!Gillian.General.Servpips.emit}
    events or {!Gillian.General.Servpips.note}s.

    The state must be treated functionally: return the (possibly updated)
    state in each branch. When returning several branches built from the same
    input state, copy it first ([E.State.copy]) so that the branches do not
    share mutable parts.

    {2 Example}

    {[
      (* __servpips_is_str(v): true iff v is a string (forks when unknown). *)
      let is_str : ServpipsExterns.handler =
        {
          run =
            (fun (type st vt)
                 (module E : ServpipsExterns.ENV
                   with type st = st
                    and type vt = vt)
                 (state : st)
                 (args : vt list) ->
              let module Expr = Gillian.Gil_syntax.Expr in
              let v =
                match args with
                | v :: _ -> v
                | [] -> E.Val.from_literal Undefined
              in
              let is_str =
                Expr.BinOp
                  (Expr.UnOp (TypeOf, E.Val.to_expr v), Equal,
                   Expr.Lit (Type StringType))
              in
              let branch f b =
                match E.Val.from_expr f with
                | None -> []
                | Some f ->
                    List.map
                      (fun st -> ServpipsExterns.Return (st, E.Val.from_literal (Bool b)))
                      (E.State.assume (E.State.copy state) f)
              in
              branch is_str true @ branch (Expr.UnOp (Not, is_str)) false);
        }

      let () = ServpipsExterns.register "servpips_is_str" is_str
    ]}

    Registration happens when the registering module is initialised, i.e.
    only if it is linked into the executable: register from a module that is
    referenced by [External.ml] (or from this module's list of built-ins). *)

(** What a handler can use: the interpreter's value and state modules and
    information about the call. *)
module type ENV = sig
  type st
  type vt

  module Val : Gillian.General.Val.S with type t = vt

  module State :
    Gillian.General.State.S with type t = st and type vt = vt

  (** [true] under symbolic execution ([wpst], [verify], [act]), [false] under
      concrete execution ([exec]). *)
  val symbolic : bool

  (** Full extern name, e.g. ["servpips_echo"]. *)
  val extern : string

  (** Identifier of the JSIL procedure containing the call. *)
  val proc : string

  (** Index of the call command in that procedure. *)
  val idx : int
end

type ('st, 'vt) outcome =
  | Return of 'st * 'vt
      (** continue after the call; the value is the call's result *)
  | Throw of 'st * 'vt
      (** the call throws the value (goes to the JS error handler) *)

type handler = {
  run :
    'st 'vt.
    (module ENV with type st = 'st and type vt = 'vt) ->
    'st ->
    'vt list ->
    ('st, 'vt) outcome list;
}

(** [register name h] registers [h] for the extern [name] (which must start
    with ["servpips_"]; JS name [__servpips_x] = extern [servpips_x]). A later
    registration for the same name replaces the earlier one. *)
val register : string -> handler -> unit

val find : string -> handler option

(** Names of all registered externs, sorted. *)
val registered : unit -> string list

(** Prefix of the extern names forwarded to the registry: ["servpips_"]. *)
val prefix : string

(** [is_servpips_extern name]: [name] starts with {!prefix}. *)
val is_servpips_extern : string -> bool

(** {2 Built-in handlers}

    - [servpips_echo(v, ...)]: returns its first argument ([undefined] if
      none). For tests.
    - [servpips_log(tag, v1, ..., vn)]: the prototype's logging extern (JS
      [__servpips_log]). Appends one JSON line per configuration to the file
      named by the environment variable [SERVPIPS_LOG] (default
      [servpips_log.jsonl]) with the arguments (objects walked one level at a
      time, up to depth 8), the path condition and the type environment;
      returns [undefined]. Independent of [--servpips]. *)

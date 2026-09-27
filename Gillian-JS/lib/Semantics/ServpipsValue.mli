(** SERVPIPS value trees (SERVPIPS design section 5.1 "VT", E17).

    [serialize heap pfs gamma v] describes the JS value [v] of the current
    path without side effects (no materialisation, no member creation):

    - [{"t":"val","e":E}]: primitives and symbolic values;
    - [{"t":"lazy","lvar":x,"aloc":#loc|null,"written":[[k,VT]...],
      "deleted":[k...]}]: a lazy value ([aloc] null when not materialised on
      this path). [written]: keys the program wrote (E15 order), plus lazily
      created members whose lazy value was written on this path (their tree
      carries the nested writes); keys made non-enumerable are reported as
      deleted;
    - [{"t":"obj","aloc":..,"props":[[k,VT]...],"sym":[[E,VT]...]}]: a
      program object: own enumerable data properties in ES2020 order
      (accessors give [opaque "accessor"]; reserved [__sp$] names are
      skipped); [sym] lists the properties with symbolic names;
    - [{"t":"arr","aloc":..,"items":[VT...],"len":null|E}]: a program array
      (holes are [undefined]; symbolic length: no items, [len] = E);
    - [{"t":"blob","src":E|null,"enc":..|null}]: an object whose metadata has
      [@sp_kind = "blob"] (fields [__sp$src], [__sp$enc]);
    - [{"t":"opaque","what":..}]: functions (["function"]), [@sp_kind] date,
      stream, set, [@sp_model] objects (["model:<name>"], ["model:object"]),
      [@sp_open] views (["model:open"]), cycles, depth > 32, and the
      fail-closed cases ["model:symbolic-attribute"] (symbolic enumerable
      flag), ["model:unknown-descriptor"], ["model:missing"]. *)

open Gillian.Gil_syntax

val serialize :
  SHeap.t ->
  Gillian.Symbolic.Pure_context.t ->
  Gillian.Symbolic.Type_env.t ->
  Expr.t ->
  Yojson.Safe.t

val serialize_ms : ServpipsLazy.mstate -> Expr.t -> Yojson.Safe.t

(** Extern [servpips_debug_vt(tag, v, withPc?)] (debug/test aid, not part of
    I2-SF): emits [{"ev":"note","code":"vt","msg":tag,"site":null,
    "data":{"vt":VT}}]; when [withPc] is [true], [data] also has ["pc"] and
    ["types"] of the current state (as in [call] events). *)
val x_debug_vt : ServpipsExterns.handler

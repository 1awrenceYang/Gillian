(** SERVPIPS LazyJSON v2: lazily materialised JSON input values (SERVPIPS
    design, section 3.2 and E2; interface I2-OCaml of section 5.2).

    Everything here is inactive until a lazy value is registered (which only
    the SERVPIPS externs do, and only under [--servpips]); Gillian then
    behaves exactly as upstream.

    {1 Model}

    A {e lazy value} is a fresh logical variable [x] registered with a name
    (e.g. ["event"], ["JSON.parse(event.body)"]), a shape (interface I3) and a
    kind. The registry is global and immutable; it is keyed by the lvar.
    Every lazy variable carries its JS type mask as a pure fact (or as a
    typing-environment entry when the mask is a single type).

    {b Materialisation.} The first memory action whose location argument is an
    unresolved registered [x] ([GetCell], [SetCell], [DeleteCell],
    [GetMetadata], [SetMetadata], [GetAllProps], and the [Sp*] actions below
    that take an object) branches over the {e classes} of [x]: one branch per
    class, each adding [x == #loc] (and the class guard, and for arrays of
    symbolic length the facts about [len(<name>)]) to the path condition. The
    abstract location of (x, class) is allocated once for the whole run, so
    the same location denotes the same object on every path, and a
    [{"ev":"lazy",...}] event is emitted once. The object's metadata holds
    [@class], [@proto], [@extensible = true], [@sp_lazy = "<lvar>"] (a string,
    immune to substitution), [@sp_lazykeys = {{ }}], and for classes with a
    resolver [@sp_resolver] and [@sp_open]. If the path condition allows [x]
    not to be an object of one of the classes (the residual
    [not (typeOf x = Obj) \/ not (g1 \/ ... \/ gn)] is satisfiable) the
    configuration ends as [unsupported].

    {b Classes.} Derived from the shape (object shape: one ["Object"] class;
    array shape: one ["Array"] class; [json]/[any]/[ddb-out]: an open JSON
    ["Object"] class and a JSON ["Array"] class; unions: the union), or given
    by JS as the [classes] argument of [__servpips_lazy]: an array of objects
    [{label, cls: "Object"|"Array", proto, resolver, guard, open}] (the
    guards must cover every object case: otherwise the residual check above
    ends the path). A class with a resolver is a {e view}: its object starts
    with a known empty domain, members are never created by LazyJSON (the
    JSIL runtime calls the resolver, WP3), writes are still tracked.

    {b Members} of a materialised JSON object at a concrete key [k] (on a
    [GetCell] miss): keys starting with [@] and keys excluded by a closed
    struct are absent (a [none] tombstone is stored); an optional key that is
    an [Object.prototype] name forks into "own" (value [<> undefined]) and
    "absent" (tombstone, value [= undefined]); any other key gets the child
    variable [memberPath(name, k)] (global memo: one variable and one [decl]
    per (x, k) for the whole run), whose mask is the member shape (plus
    [undefined] when optional), stored as [{{"d", v, true, true, true}}] and
    added to [@sp_lazykeys]. No fork.

    {b Arrays.} Fixed length (shape [len: L], e.g. Records shards): the
    [length] cell is [L]; indices [0 .. L-1] are created lazily, every other
    name is absent. Symbolic length: [length] is the variable [len(<name>)]
    ([is_int], [0 <= len <= 2^32-1], shape [minLen]/[maxLen]; one [decl] of
    kind ["length"]); reading index [i] forks on [i < len] (child created) /
    [len <= i] (absent). A symbolic index [ToString(e)] on an unwritten array
    forks into "some element exists" (fresh skolem [elem(<name>)#k], element
    mask, no fact about the index) and "no element" (both store a symbolic
    cell; any later write to that array is [unsupported]). Other symbolic
    names on arrays, and symbolic names on JSON objects, are [unsupported].

    {b Writes} ([SetCell], including [none] for JS [delete], and
    [DeleteCell]) on a lazy object: a symbolic name is [unsupported];
    otherwise the key leaves [@sp_lazykeys] and enters [@sp_written] (value)
    or [@sp_deleted] ([none]); the object and all its registry ancestors
    become {e dirty} on this path. All this bookkeeping lives in the heap
    (object metadata, and the reserved object [$lsp_lazy_state] for
    dirtiness), so it is copied with the state and is private to a path.

    {b Enumeration} ([GetAllProps]) of a lazy JSON object, of a view, or of
    any object whose metadata has [@sp_open] set: [unsupported]. A lazy array
    whose length is concrete is enumerated exactly (its missing elements are
    created first). Order: ES2020 OrdinaryOwnPropertyKeys (E15, see
    [SHeap.ordered_fields]).

    {b Prefetched members} ([member]): the child variable of [x] at [k]
    without materialising [x] (or of the materialised object at [k] when the
    key was not written). The same variable fills the cell if [x] is later
    materialised as an object; [undefined] when no class admits [k]. On a
    view (an object of a class with a resolver) it is the member of the
    underlying input value given by the class's member structure: this is
    how a resolver reads the input it presents (e.g. the string member
    [Body] of an S3 response, wrapped in a Buffer).
    [SpDefine] of [length] on an object of class [Array] writes the array
    length descriptor (writable, not enumerable, not configurable). The
    value tree of a view reports the keys its resolver defined with a value
    other than the input's own member as written (see {!ServpipsValue}).

    {1 Memory actions}

    Extern handlers cannot reach this module directly (their state type is
    abstract), so the operations are also JSILSMemory actions, reachable
    through [E.State.execute_action] (see {!Ext}):

    - [SpLazy(name, shapeId, kind, classes)] -> [[x]]: register (the four
      first arguments must be string literals; [classes] is [undefined] or a
      JS array). Emits the root [decl].
    - [SpMember(x, key)] -> [[child]]
    - [SpIsLazy(v, "pristine"|"any")] -> [[bool]]: [v] is a lazy value (an
      lvar or a materialised object) and, for ["pristine"], neither it nor
      any descendant was written on this path.
    - [SpLazyName(v)] -> [[name | undefined]]
    - [SpSerialize(v)] -> [[id]]: value tree of [v] (E17, see
      {!ServpipsValue}); fetch it with {!take_serialized}.
    - [SpMarkLazyKey(o, key)] -> [[]]: add [key] to [@sp_lazykeys] of [o].
    - [SpDefine(o, key, v)] -> [[]]: raw write of [{{"d", v, true, true,
      true}}] (keeping the domain invariant) and [SpMarkLazyKey]; not a
      program write (no dirtiness). For resolvers ([__sp.define]).
    - [SpAbsent(o, key)] -> [[]]: raw tombstone ([__sp.absent]).

    {1 Externs registered here}

    [servpips_lazy], [servpips_member], [servpips_is_lazy] (optional second
    argument ["pristine"] (default) or ["any"]), [servpips_shapes], and the
    addition [servpips_lazy_name(v)] (name string or [undefined]). *)

open Gillian.Gil_syntax
module PFS = Gillian.Symbolic.Pure_context
module Type_env = Gillian.Symbolic.Type_env

(** {1 Shapes (I3)} *)

(** Register shapes: either an I3 document [{"v":2,"shapes":{...},...}] or a
    bare [{id: shape}] map. Later registrations replace earlier ids. Built-in
    ids usable without a table: [json any ddb-out string number boolean null
    object array absent]. Shape keys used: [type], [ref], [props],
    [required], [additional], [closed], [items], [len], [minLen], [maxLen],
    [enum], [const], [optional], [nullable], [of]; others are ignored (only
    weakening the masks). Unknown [type]s make every use [unsupported]. *)
val set_shapes : Yojson.Safe.t -> unit

(** Parse and register a shape table text (see the .ml for the compiler's
    string-literal escaping workaround). *)
val set_shapes_text : string -> unit

(** {1 Registry} *)

type cls = Obj_cls | Arr_cls

type class_spec = {
  label : string;
  cls : cls;
  proto : Expr.t;
  resolver : Expr.t option;
  guard : Expr.t;
  open_ : bool;
  members : Yojson.Safe.t;
}

type info = {
  lvar : string;
  name : string;
  kind : string;
  shape : Yojson.Safe.t;
  label : Yojson.Safe.t;
  classes : class_spec array;
  parent : (string * string option) option;
      (** parent lvar and key ([None] key for array skolems) *)
  mask : Expr.t list;  (** disjuncts of the type mask over [LVar lvar] *)
  gamma_type : Type.t option;
}

(** Has any lazy value been registered? *)
val active : unit -> bool

val find : string -> info option

(** The (lvar, class index) materialised at an abstract location. *)
val owner_of_aloc : string -> (string * int) option

val is_lazy_aloc : string -> bool
val may_be_object : info -> bool

(** [memberPath(base, key)] of the naming convention I5. *)
val member_path : string -> string -> string

(** {1 Memory-level operations}

    The frozen I2-OCaml signatures take the symbolic state; here the state is
    the memory-level triple, which is what memory actions receive. *)

type mstate = { heap : SHeap.t; pfs : PFS.t; gamma : Type_env.t }

(** Successors of a memory action: heap, returned values, new pure facts, new
    types. *)
type ret = (SHeap.t * Expr.t list * Expr.t list * (string * Type.t) list) list

val register :
  mstate ->
  name:string ->
  shape:string ->
  kind:string ->
  classes:Expr.t ->
  parent:(string * string option) option ->
  Expr.t * Expr.t list * (string * Type.t) list

val member : mstate -> Expr.t -> Expr.t -> Expr.t * Expr.t list * (string * Type.t) list
val is_lazy : mstate -> ?any:bool -> Expr.t -> bool
val lazy_name : mstate -> Expr.t -> string option
val mark_lazy_key : mstate -> loc:string -> key:string -> unit
val define : mstate -> loc:string -> key:string -> Expr.t -> unit
val absent : mstate -> loc:string -> key:string -> unit

(** {2 Hooks used by JSILSMemory} *)

type branch = SHeap.t * Expr.t list * (string * Type.t) list * string

(** Branches (heap, facts, types, location) if the location is an unresolved
    registered lazy value. *)
val materialize_loc : mstate -> Expr.t -> branch list option

(** [GetCell] miss on a lazy object ([None]: not a LazyJSON object). *)
val get_cell_miss : mstate -> string -> Expr.t -> ret option

(** Bookkeeping before a [SetCell] (value [none] = delete). *)
val before_set_cell : mstate -> string -> Expr.t -> Expr.t -> unit

(** [GetAllProps] hook: [Some (names, facts, types)] for lazy arrays, raises
    [Path_end] for open objects, [None] otherwise. *)
val get_all_props :
  mstate -> string -> (Expr.t list * Expr.t list * (string * Type.t) list) option

(** {2 Helpers shared with ServpipsValue} *)

val str : string -> Expr.t
val reduce : mstate -> Expr.t -> Expr.t

(** A literal equal to the expression on this path, when the reduction or an
    equality of the path condition gives one. *)
val concrete_of : mstate -> Expr.t -> Expr.t option
val fvl_of : SHeap.t -> string -> SFVL.t
val cell : SHeap.t -> string -> Expr.t -> Expr.t option
val meta_cell : SHeap.t -> string -> string -> Expr.t option
val js_prop : SHeap.t -> string -> string -> Expr.t option
val string_set : SHeap.t -> string -> string -> string list
val loc_name_of : mstate -> Expr.t -> string option
val lazy_of_value : mstate -> Expr.t -> (info * (string * int) option) option
val is_dirty : SHeap.t -> string -> bool

(** Children of a lazy value in creation order: (key, child lvar), key
    [None] for arbitrary array elements. *)
val children_list : string -> (string option * string) list

(** Member shape (and optionality) of a key in the member structure of a
    class, whether or not the class has a resolver (for a view: the
    structure of the underlying input value). *)
val class_struct_member : class_spec -> string -> (Yojson.Safe.t * bool) option

(** Member LazyJSON creates itself for a key (on a [GetCell] miss): as
    {!class_struct_member}, but none for a class with a resolver. *)
val class_member : class_spec -> string -> (Yojson.Safe.t * bool) option
val lazykeys_key : string
val written_key : string
val deleted_key : string

(** {1 Memory action names} *)

val a_lazy : string
val a_member : string
val a_is_lazy : string
val a_lazy_name : string
val a_mark_lazy_key : string
val a_define : string
val a_absent : string
val a_serialize : string
val stash_serialized : Yojson.Safe.t -> int
val take_serialized : int -> Yojson.Safe.t option

(** {1 Extern-level API (abstract states)}

    For extern handlers (e.g. WP3's [__servpips_emit("call", ...)] uses
    {!Ext.serialize}, [__servpips_define] uses {!Ext.define}). All raise
    [Servpips.Path_end] ([unsupported]) under concrete execution, except
    [is_lazy] / [lazy_name] which answer [false] / [None] and [serialize]. *)
module Ext : sig
  type ('st, 'vt) env =
    (module ServpipsExterns.ENV with type st = 'st and type vt = 'vt)

  val register :
    ('st, 'vt) env ->
    'st ->
    name:string ->
    shape:string ->
    kind:string ->
    classes:'vt ->
    'st * 'vt

  val member : ('st, 'vt) env -> 'st -> 'vt -> 'vt -> 'st * 'vt
  val is_lazy : ('st, 'vt) env -> 'st -> ?any:bool -> 'vt -> bool
  val lazy_name : ('st, 'vt) env -> 'st -> 'vt -> string option
  val mark_lazy_key : ('st, 'vt) env -> 'st -> loc:'vt -> key:'vt -> 'st list
  val define : ('st, 'vt) env -> 'st -> 'vt -> 'vt -> 'vt -> 'st list
  val absent : ('st, 'vt) env -> 'st -> 'vt -> 'vt -> 'st list

  (** Value tree (I1 VT) of a value. Symbolic execution: [SpSerialize]
      (ServpipsValue). Concrete execution ([exec], no lazy values): the same
      format built through GetMetadata/GetCell/GetAllProps, with the concrete
      memory's property order (not E15). *)
  val serialize : ('st, 'vt) env -> 'st -> 'vt -> Yojson.Safe.t
end

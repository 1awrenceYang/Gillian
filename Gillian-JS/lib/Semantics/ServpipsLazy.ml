(* SERVPIPS LazyJSON v2 (design section 3.2, E2). See ServpipsLazy.mli. *)

open Gillian.Gil_syntax
open Js2jsil_lib
module PFS = Gillian.Symbolic.Pure_context
module Type_env = Gillian.Symbolic.Type_env
module FOSolver = Gillian.Logic.FOSolver
module Reduction = Gillian.Logic.Reduction
module Servpips = Gillian.General.Servpips
module J = Yojson.Safe

(* ------------------------------------------------------------------------ *)
(* Failures                                                                 *)
(* ------------------------------------------------------------------------ *)

let unsupported reason =
  raise (Servpips.Path_end { status = "unsupported"; reason })

let engine_error reason = raise (Servpips.Path_end { status = "error"; reason })

(* ------------------------------------------------------------------------ *)
(* Small expression helpers                                                 *)
(* ------------------------------------------------------------------------ *)

let str s = Expr.Lit (String s)
let true_ = Expr.Lit (Bool true)
let undef = Expr.Lit Undefined
let null = Expr.Lit Null
let nono = Expr.Lit Nono
let eq a b = Expr.BinOp (a, Equal, b)
let not_ a = Expr.UnOp (Not, a)
let type_atom x t = eq (Expr.UnOp (TypeOf, x)) (Expr.Lit (Type t))

let disj = function
  | [] -> Expr.Lit (Bool false)
  | [ x ] -> x
  | x :: rest -> List.fold_left (fun ac y -> Expr.BinOp (ac, Or, y)) x rest

let data_desc v = Expr.EList [ str "d"; v; true_; true_; true_ ]
let loc_expr name = Expr.loc_from_loc_name name
let obj_proto = Expr.Lit (Loc JS2JSIL_Helpers.locObjPrototype)
let arr_proto = Expr.Lit (Loc JS2JSIL_Helpers.locArrPrototype)

(* ------------------------------------------------------------------------ *)
(* Memory-level state                                                       *)
(* ------------------------------------------------------------------------ *)

type mstate = { heap : SHeap.t; pfs : PFS.t; gamma : Type_env.t }

type ret =
  (SHeap.t * Expr.t list * Expr.t list * (string * Type.t) list) list

let sat (ms : mstate) ?(gamma = ms.gamma) (fs : Expr.t list) : bool =
  FOSolver.check_satisfiability ~time:"SERVPIPS lazy"
    (fs @ PFS.to_list ms.pfs)
    gamma

let reduce (ms : mstate) (e : Expr.t) : Expr.t =
  try Reduction.reduce_lexpr ~pfs:ms.pfs ~gamma:ms.gamma e with _ -> e

(** A literal equal to [e] on this path, if the reduction or an equality of
    the path condition gives one. *)
let concrete_of (ms : mstate) (e : Expr.t) : Expr.t option =
  match reduce ms e with
  | Lit _ as l -> Some l
  | e' ->
      PFS.fold_left
        (fun ac f ->
          match ac with
          | Some _ -> ac
          | None -> (
              let is_e a = Expr.equal a e || Expr.equal a e' in
              match f with
              | Expr.BinOp (a, Equal, (Lit _ as l)) when is_e a -> Some l
              | Expr.BinOp ((Lit _ as l), Equal, a) when is_e a -> Some l
              | _ -> None))
        None ms.pfs

let gamma_with (ms : mstate) (types : (string * Type.t) list) : Type_env.t =
  match types with
  | [] -> ms.gamma
  | _ ->
      let g = Type_env.copy ms.gamma in
      List.iter (fun (x, t) -> Type_env.update g x t) types;
      g

(* ------------------------------------------------------------------------ *)
(* JSON helpers and names                                                   *)
(* ------------------------------------------------------------------------ *)

let field k (j : J.t) =
  match j with
  | `Assoc l -> List.assoc_opt k l
  | _ -> None

let str_field k j =
  match field k j with
  | Some (`String s) -> Some s
  | _ -> None

let bool_field k j =
  match field k j with
  | Some (`Bool b) -> b
  | _ -> false

let set_field k v (j : J.t) : J.t =
  match j with
  | `Assoc l -> `Assoc ((k, v) :: List.remove_assoc k l)
  | _ -> j

(** JS [JSON.stringify] of a string (ASCII escapes as in V8). *)
let json_quote (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let is_ident (s : string) : bool =
  String.length s > 0
  && (match s.[0] with
     | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
     | _ -> false)
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true
         | _ -> false)
       s

let is_index_form (s : string) : bool =
  String.length s > 0
  && String.for_all (fun c -> c >= '0' && c <= '9') s
  && (String.length s = 1 || s.[0] <> '0')

(** [memberPath] of the SERVPIPS naming (I5), identical to
    [engine/values.js memberPath]. *)
let member_path (base : string) (key : string) : string =
  if is_index_form key then Printf.sprintf "%s[%s]" base key
  else if is_ident key then base ^ "." ^ key
  else Printf.sprintf "%s[%s]" base (json_quote key)

let object_prototype_names =
  [
    "constructor";
    "toString";
    "toLocaleString";
    "valueOf";
    "hasOwnProperty";
    "isPrototypeOf";
    "propertyIsEnumerable";
    "__proto__";
    "__defineGetter__";
    "__defineSetter__";
    "__lookupGetter__";
    "__lookupSetter__";
  ]

(* ------------------------------------------------------------------------ *)
(* Shapes (I3)                                                              *)
(* ------------------------------------------------------------------------ *)

let shape_table : (string, J.t) Hashtbl.t = Hashtbl.create 64

let builtin_shape_ids =
  [
    "json";
    "any";
    "ddb-out";
    "string";
    "number";
    "boolean";
    "null";
    "object";
    "array";
    "absent";
  ]

let set_shapes (j : J.t) : unit =
  let entries =
    match j with
    | `Assoc l -> (
        match List.assoc_opt "shapes" l with
        | Some (`Assoc s) -> s
        | Some _ -> invalid_arg "servpips shapes: \"shapes\" is not an object"
        | None -> l)
    | _ -> invalid_arg "servpips shapes: not a JSON object"
  in
  List.iter (fun (k, v) -> Hashtbl.replace shape_table k v) entries

(** The JS compiler currently stores every JS string literal with each ["]
    replaced by [\"] (JS2JSIL_Compiler.ml, [String] literals). If the text
    does not parse as JSON, it is parsed again with that escaping undone
    (the exact inverse: every [\"] becomes ["], left to right). *)
let set_shapes_text (text : string) : unit =
  let j =
    try J.from_string text
    with J.Util.Type_error _ | Yojson.Json_error _ ->
      J.from_string (Str.global_replace (Str.regexp_string "\\\"") "\"" text)
  in
  set_shapes j

let rec resolve ?(depth = 0) (s : J.t) : J.t =
  if depth > 64 then unsupported "shape reference cycle";
  match str_field "ref" s with
  | None -> s
  | Some id -> (
      let target =
        match Hashtbl.find_opt shape_table id with
        | Some t -> t
        | None ->
            if List.mem id builtin_shape_ids then `Assoc [ ("type", `String id) ]
            else unsupported ("unknown shape " ^ id)
      in
      let target = resolve ~depth:(depth + 1) target in
      match (s, target) with
      | `Assoc l, `Assoc t ->
          let own = List.filter (fun (k, _) -> k <> "ref") l in
          `Assoc (own @ List.filter (fun (k, _) -> not (List.mem_assoc k own)) t)
      | _ -> target)

let shape_of_id (id : string) : J.t =
  match Hashtbl.find_opt shape_table id with
  | Some s -> resolve s
  | None ->
      if List.mem id builtin_shape_ids then `Assoc [ ("type", `String id) ]
      else unsupported ("unknown shape id " ^ id)

(** The label of a (member) shape for [decl.shape]: the id of a reference, or
    the inline shape itself. *)
let shape_label (s : J.t) : J.t =
  match str_field "ref" s with
  | Some id -> `String id
  | None -> s

let type_of (s : J.t) : string =
  match str_field "type" s with
  | Some t -> t
  | None -> if field "of" s <> None then "union" else "any"

let is_absent_shape (s : J.t) : bool =
  match s with
  | `String "absent" -> true
  | `Assoc _ -> type_of (resolve s) = "absent"
  | _ -> false

let lit_of_json : J.t -> Expr.t option = function
  | `String s -> Some (str s)
  | `Int n -> Some (Expr.Lit (Num (float_of_int n)))
  | `Intlit s -> Option.map (fun f -> Expr.Lit (Num f)) (float_of_string_opt s)
  | `Float f -> Some (Expr.Lit (Num f))
  | `Bool b -> Some (Expr.Lit (Bool b))
  | `Null -> Some null
  | _ -> None

let enum_values (s : J.t) : Expr.t list option =
  let lits l =
    List.map
      (fun j ->
        match lit_of_json j with
        | Some e -> e
        | None -> unsupported "non-literal enum value in shape")
      l
  in
  match field "enum" s with
  | Some (`List l) -> Some (lits l)
  | _ -> (
      match field "const" s with
      | Some j -> Some (lits [ j ])
      | None -> None)

(** Disjuncts of the JS type mask of a value of shape [s] (named [x]). The
    constraints used: [type], [enum]/[const], [optional], [nullable],
    [union.of]; all other keys (pattern, string lengths, defaults, ...) are
    ignored, which only weakens the mask. *)
let rec mask_disjuncts (s : J.t) (x : Expr.t) : Expr.t list =
  let s = resolve s in
  let enum_or dflt =
    match enum_values s with
    | Some vs -> List.map (eq x) vs
    | None -> dflt
  in
  let json_d =
    [
      type_atom x StringType;
      type_atom x NumberType;
      type_atom x BooleanType;
      eq x null;
      type_atom x ObjectType;
    ]
  in
  let base =
    match type_of s with
    | "string" -> enum_or [ type_atom x StringType ]
    | "number" -> enum_or [ type_atom x NumberType ]
    | "boolean" -> enum_or [ type_atom x BooleanType ]
    | "null" -> [ eq x null ]
    | "absent" -> [ eq x undef ]
    | "object" | "array" -> [ type_atom x ObjectType ]
    | "json" -> json_d
    | "any" -> json_d @ [ eq x undef ]
    | "ddb-out" -> json_d @ [ eq x undef ]
    | "union" -> (
        match field "of" s with
        | Some (`List l) -> List.concat_map (fun s' -> mask_disjuncts s' x) l
        | _ -> unsupported "union shape without \"of\"")
    | t -> unsupported ("unsupported shape type " ^ t)
  in
  let extra =
    (if bool_field "optional" s then [ eq x undef ] else [])
    @ if bool_field "nullable" s then [ eq x null ] else []
  in
  List.sort_uniq Expr.compare (base @ extra)

(* ------------------------------------------------------------------------ *)
(* Classes                                                                  *)
(* ------------------------------------------------------------------------ *)

type cls = Obj_cls | Arr_cls

type class_spec = {
  label : string;
  cls : cls;
  proto : Expr.t;
  resolver : Expr.t option;
  guard : Expr.t;
  open_ : bool;
  members : J.t;
      (** member structure: an object shape (props / additional / required /
          closed) for [Obj_cls], an array shape (items / len / minLen /
          maxLen) for [Arr_cls]; unused when [resolver] is set *)
}

let cls_name = function
  | Obj_cls -> "Object"
  | Arr_cls -> "Array"

let json_object_shape : J.t =
  `Assoc
    [
      ("type", `String "object");
      ("additional", `Assoc [ ("type", `String "json"); ("optional", `Bool true) ]);
    ]

let json_array_shape : J.t =
  `Assoc [ ("type", `String "array"); ("items", `Assoc [ ("type", `String "json") ]) ]

let default_class cls members =
  {
    label = "json";
    cls;
    proto = (if cls = Obj_cls then obj_proto else arr_proto);
    resolver = None;
    guard = true_;
    open_ = false;
    members;
  }

(** Classes of a value of shape [s] when it is an object (section 3.2:
    derived from the shape; guard true). *)
let rec shape_classes (s : J.t) : class_spec list =
  let s = resolve s in
  match type_of s with
  | "object" -> [ default_class Obj_cls s ]
  | "array" -> [ default_class Arr_cls s ]
  | "json" | "any" | "ddb-out" ->
      [ default_class Obj_cls json_object_shape; default_class Arr_cls json_array_shape ]
  | "union" -> (
      match field "of" s with
      | Some (`List l) -> List.concat_map shape_classes l
      | _ -> [])
  | _ -> []

(** Is the object shape closed (no members beyond [props])? *)
let is_closed (s : J.t) : bool =
  bool_field "closed" s
  ||
  match field "additional" s with
  | Some a -> is_absent_shape a
  | None -> false

(** Member rule of an object shape for the concrete key [k]:
    [None]: the key is never an own member; [Some (shape, optional)]. *)
let obj_member_rule (s : J.t) (k : string) : (J.t * bool) option =
  let props =
    match field "props" s with
    | Some (`Assoc l) -> l
    | _ -> []
  in
  match List.assoc_opt k props with
  | Some m ->
      if is_absent_shape m then None
      else
        let required =
          match field "required" s with
          | Some (`List l) -> Some (List.filter_map (function `String x -> Some x | _ -> None) l)
          | _ -> None
        in
        let rm = resolve m in
        let optional =
          bool_field "optional" rm
          ||
          match required with
          | Some r -> not (List.mem k r)
          | None -> false
        in
        Some (m, optional)
  | None -> (
      if is_closed s then None
      else
        match field "additional" s with
        | Some a -> if is_absent_shape a then None else Some (a, true)
        | None -> Some (`Assoc [ ("type", `String "any") ], true))

type arr_len = Fixed of int | Sym

let array_len_kind (s : J.t) : arr_len =
  match field "len" s with
  | Some (`Int n) when n >= 0 -> Fixed n
  | Some (`Float f) when Float.is_integer f && f >= 0. -> Fixed (int_of_float f)
  | _ -> Sym

let array_items (s : J.t) : J.t =
  match field "items" s with
  | Some i -> i
  | None -> `Assoc [ ("type", `String "json") ]

(* ------------------------------------------------------------------------ *)
(* Registry                                                                 *)
(* ------------------------------------------------------------------------ *)

type info = {
  lvar : string;
  name : string;
  kind : string;
  shape : J.t;
  label : J.t;
  classes : class_spec array;
  parent : (string * string option) option;
  mask : Expr.t list;
  gamma_type : Type.t option;
}

let infos : (string, info) Hashtbl.t = Hashtbl.create 256
let children : (string * string, string) Hashtbl.t = Hashtbl.create 256
let mat_alocs : (string * int, string * string) Hashtbl.t = Hashtbl.create 256
let aloc_owner : (string, string * int) Hashtbl.t = Hashtbl.create 256
let len_vars : (string, string) Hashtbl.t = Hashtbl.create 64
let elem_counter : (string, int) Hashtbl.t = Hashtbl.create 64
let active () = Hashtbl.length infos > 0
let find (x : string) : info option = Hashtbl.find_opt infos x
let owner_of_aloc (l : string) : (string * int) option = Hashtbl.find_opt aloc_owner l
let is_lazy_aloc (l : string) : bool = Hashtbl.mem aloc_owner l

let () = SHeap.keep_hook := is_lazy_aloc

let may_be_object (info : info) = Array.length info.classes > 0

let open_json (info : info) : J.t =
  let objs = List.filter (fun c -> c.cls = Obj_cls) (Array.to_list info.classes) in
  match objs with
  | [] -> if Array.length info.classes > 0 then `Bool false else `Null
  | _ ->
      `Bool
        (List.exists (fun c -> c.resolver <> None || c.open_ || not (is_closed c.members)) objs)

let opt_str = function
  | Some s -> `String s
  | None -> `Null

let emit_decl ?(sort = "JS") ?parent_name ?key ?parent_lvar ?parent_aloc
    ?(shape : J.t option) ?(open_ : J.t option) ~lvar ~name ~kind () : unit =
  Servpips.emit
    (`Assoc
      [
        ("ev", `String "decl");
        ("lvar", `String lvar);
        ("name", `String name);
        ("sort", `String sort);
        ("kind", `String kind);
        ("parent", opt_str parent_name);
        ("key", opt_str key);
        ("parent_lvar", opt_str parent_lvar);
        ("parent_aloc", opt_str parent_aloc);
        ("shape", Option.value ~default:`Null shape);
        ("open", Option.value ~default:`Null open_);
        ("site", `Null);
        ("k", `Null);
      ])

let decl_info ?parent_aloc (info : info) : unit =
  let parent_name, parent_lvar, key =
    match info.parent with
    | None -> (None, None, None)
    | Some (p, k) -> (
        match find p with
        | Some pi -> (Some pi.name, Some p, k)
        | None -> (None, Some p, k))
  in
  emit_decl ?parent_name ?key ?parent_lvar ?parent_aloc ~shape:info.label
    ~open_:(open_json info) ~lvar:info.lvar ~name:info.name ~kind:info.kind ()

let single_gamma_type = function
  | [ Expr.BinOp (UnOp (TypeOf, _), Equal, Lit (Type t)) ] -> (
      match t with
      | StringType | NumberType | BooleanType | ObjectType -> Some t
      | _ -> None)
  | _ -> None

let new_info ~name ~kind ~(shape : J.t) ~(label : J.t) ?classes ~parent () : info =
  let lvar = LVar.alloc () in
  let x = Expr.LVar lvar in
  let mask = mask_disjuncts shape x in
  let may_obj = List.mem (type_atom x ObjectType) mask in
  let classes =
    if not may_obj then [||]
    else
      match classes with
      | Some (_ :: _ as cs) -> Array.of_list cs
      | _ -> Array.of_list (shape_classes shape)
  in
  let info =
    {
      lvar;
      name;
      kind;
      shape = resolve shape;
      label;
      classes;
      parent;
      mask;
      gamma_type = single_gamma_type mask;
    }
  in
  Hashtbl.replace infos lvar info;
  info

(** Facts making the type mask of [info] hold on the current path (nothing if
    already there). *)
let mask_facts (ms : mstate) (info : info) : Expr.t list * (string * Type.t) list =
  match info.gamma_type with
  | Some t ->
      if Type_env.get ms.gamma info.lvar = Some t then ([], [])
      else ([], [ (info.lvar, t) ])
  | None ->
      let f = disj info.mask in
      if PFS.mem ms.pfs f then ([], []) else ([ f ], [])

(* ------------------------------------------------------------------------ *)
(* Heap helpers                                                             *)
(* ------------------------------------------------------------------------ *)

let fvl_of (heap : SHeap.t) (loc : string) : SFVL.t =
  match SHeap.get heap loc with
  | Some ((fvl, _), _) -> fvl
  | None -> SFVL.empty

let cell (heap : SHeap.t) (loc : string) (k : Expr.t) : Expr.t option =
  SFVL.get k (fvl_of heap loc)

let meta_loc (heap : SHeap.t) (loc : string) : string option =
  match SHeap.get heap loc with
  | Some (_, Some (ALoc m)) | Some (_, Some (Lit (Loc m))) -> Some m
  | _ -> None

let meta_cell (heap : SHeap.t) (loc : string) (key : string) : Expr.t option =
  match meta_loc heap loc with
  | None -> None
  | Some m -> (
      match cell heap m (str key) with
      | Some (Lit Nono) | None -> None
      | v -> v)

(** Write a cell keeping the domain invariant (fields of an object with a
    domain are in the domain). *)
let raw_set_cell (heap : SHeap.t) (loc : string) (k : Expr.t) (v : Expr.t) : unit
    =
  (match SHeap.get heap loc with
  | Some ((fvl, Some dom), met) when SFVL.get k fvl = None ->
      let dom' =
        match dom with
        | ESet l -> Expr.ESet (List.sort_uniq Expr.compare (k :: l))
        | d -> Expr.NOp (SetUnion, [ d; ESet [ k ] ])
      in
      SHeap.set heap loc fvl (Some dom') met
  | _ -> ());
  SHeap.set_fv_pair heap loc k v

let set_meta_cell (heap : SHeap.t) (loc : string) (key : string) (v : Expr.t) :
    unit =
  match meta_loc heap loc with
  | Some m -> raw_set_cell heap m (str key) v
  | None -> engine_error ("SERVPIPS: object without metadata: " ^ loc)

let string_set (heap : SHeap.t) (loc : string) (key : string) : string list =
  match meta_cell heap loc key with
  | Some (ESet l) -> List.filter_map (function Expr.Lit (String s) -> Some s | _ -> None) l
  | _ -> []

let put_string_set heap loc key (l : string list) =
  set_meta_cell heap loc key
    (ESet (List.map str (List.sort_uniq String.compare l)))

let set_add heap loc key s =
  let l = string_set heap loc key in
  if not (List.mem s l) then put_string_set heap loc key (s :: l)

let set_remove heap loc key s =
  let l = string_set heap loc key in
  if List.mem s l then put_string_set heap loc key (List.filter (( <> ) s) l)

let lazykeys_key = "@sp_lazykeys"
let written_key = "@sp_written"
let deleted_key = "@sp_deleted"
let symcells_key = "@sp_symcells"

(* Path-private dirtiness of lazy values: a reserved object whose cells are
   the lvars of lazy values written (or with a written descendant) on this
   path. *)
let state_loc = "$lsp_lazy_state"

let is_dirty (heap : SHeap.t) (x : string) : bool =
  match cell heap state_loc (str x) with
  | Some (Lit (Bool true)) -> true
  | _ -> false

let mark_dirty (heap : SHeap.t) (x : string) : unit =
  if not (SHeap.has_loc heap state_loc) then
    SHeap.init_object heap state_loc None;
  let rec up x =
    if not (is_dirty heap x) then raw_set_cell heap state_loc (str x) true_;
    match find x with
    | Some { parent = Some (p, _); _ } -> up p
    | _ -> ()
  in
  up x

(* ------------------------------------------------------------------------ *)
(* Children                                                                 *)
(* ------------------------------------------------------------------------ *)

(** Member shape contributed by class [c] for key [k] (with optionality). *)
let class_member (c : class_spec) (k : string) : (J.t * bool) option =
  if c.resolver <> None then None
  else if String.length k > 0 && k.[0] = '@' then None
  else
    match c.cls with
    | Obj_cls -> obj_member_rule c.members k
    | Arr_cls -> (
        if not (SHeap.is_array_index k) then None
        else
          let items = array_items c.members in
          match array_len_kind c.members with
          | Fixed n ->
              if String.length k <= 10 && int_of_string k < n then Some (items, false)
              else None
          | Sym -> Some (items, false))

let with_optional (s : J.t) (opt : bool) : J.t =
  if not opt then s
  else
    match s with
    | `Assoc _ -> set_field "optional" (`Bool true) s
    | _ -> s

let contributions (info : info) (k : string) : (int * J.t) list =
  List.filter_map
    (fun (i, c) ->
      Option.map (fun (s, o) -> (i, with_optional s o)) (class_member c k))
    (List.mapi (fun i c -> (i, c)) (Array.to_list info.classes))

(** The child of [info] at the concrete key [k] (global memo, one decl per
    child). Its mask is the union over the classes admitting [k]. *)
let get_child ?parent_aloc (info : info) (k : string) : info =
  match Hashtbl.find_opt children (info.lvar, k) with
  | Some v -> Hashtbl.find infos v
  | None ->
      let contribs = contributions info k in
      let shape, label =
        match contribs with
        | [] -> engine_error ("SERVPIPS: no class admits member " ^ k)
        | [ (_, s) ] -> (s, shape_label s)
        | l ->
            let u = `Assoc [ ("type", `String "union"); ("of", `List (List.map snd l)) ] in
            (u, u)
      in
      let child =
        new_info ~name:(member_path info.name k) ~kind:info.kind ~shape ~label
          ~parent:(Some (info.lvar, Some k)) ()
      in
      Hashtbl.replace children (info.lvar, k) child.lvar;
      decl_info ?parent_aloc child;
      child

(** Facts for using [child] under class [i] of its parent: its mask, plus the
    class-specific restriction when several classes admit the key. *)
let child_facts (ms : mstate) (parent : info) (i : int) (k : string) (child : info)
    : Expr.t list * (string * Type.t) list * bool =
  let facts, types = mask_facts ms child in
  let contribs = contributions parent k in
  if List.length contribs <= 1 then (facts, types, false)
  else
    match List.assoc_opt i contribs with
    | Some s ->
        let r = disj (mask_disjuncts s (Expr.LVar child.lvar)) in
        if PFS.mem ms.pfs r then (facts, types, false) else (facts @ [ r ], types, true)
    | None -> (facts, types, false)

(* ------------------------------------------------------------------------ *)
(* Materialisation                                                          *)
(* ------------------------------------------------------------------------ *)

let emit_lazy ~lvar ~aloc ~cls ~label ~(len : Expr.t option) =
  Servpips.emit
    (`Assoc
      [
        ("ev", `String "lazy");
        ("lvar", `String lvar);
        ("aloc", `String aloc);
        ("class", `String (cls_name cls));
        ("label", `String label);
        ("len", match len with Some e -> Servpips.expr_json e | None -> `Null);
      ])

(** Length variable [len(<name>)] of a symbolic-length array (global memo). *)
let len_var (info : info) (c : class_spec) : string =
  match Hashtbl.find_opt len_vars info.lvar with
  | Some l -> l
  | None ->
      let l = LVar.alloc () in
      Hashtbl.replace len_vars info.lvar l;
      ignore c;
      emit_decl ~sort:"Num" ~parent_name:info.name ~key:"length"
        ~parent_lvar:info.lvar ~lvar:l
        ~name:(Printf.sprintf "len(%s)" info.name)
        ~kind:"length" ();
      l

let len_facts (c : class_spec) (l : string) : Expr.t list =
  let lv = Expr.LVar l in
  let le a b = Expr.BinOp (a, FLessThanEqual, b) in
  let num n = Expr.Lit (Num n) in
  let bound k =
    match field k c.members with
    | Some (`Int n) -> Some (float_of_int n)
    | Some (`Float f) -> Some f
    | _ -> None
  in
  [ Expr.UnOp (IsInt, lv); le (num 0.) lv; le lv (num 4294967295.) ]
  @ (match bound "minLen" with Some n -> [ le (num n) lv ] | None -> [])
  @ match bound "maxLen" with Some n -> [ le lv (num n) ] | None -> []

let array_length_expr (info : info) (c : class_spec) : Expr.t =
  match array_len_kind c.members with
  | Fixed n -> Expr.Lit (Num (float_of_int n))
  | Sym -> Expr.LVar (len_var info c)

let mat_aloc (info : info) (i : int) : string * string =
  match Hashtbl.find_opt mat_alocs (info.lvar, i) with
  | Some p -> p
  | None ->
      let c = info.classes.(i) in
      let al = ALoc.alloc () and alm = ALoc.alloc () in
      Hashtbl.replace mat_alocs (info.lvar, i) (al, alm);
      Hashtbl.replace aloc_owner al (info.lvar, i);
      let len =
        match (c.cls, c.resolver) with
        | Arr_cls, None -> Some (array_length_expr info c)
        | _ -> None
      in
      emit_lazy ~lvar:info.lvar ~aloc:al ~cls:c.cls ~label:c.label ~len;
      (al, alm)

let create_object (heap : SHeap.t) (info : info) (c : class_spec) (al : string)
    (alm : string) : unit =
  let fvl, dom =
    match (c.cls, c.resolver) with
    | _, Some _ -> (SFVL.empty, Some (Expr.ESet []))
    | Obj_cls, None -> (SFVL.empty, None)
    | Arr_cls, None ->
        let len = array_length_expr info c in
        ( SFVL.add (str "length")
            (Expr.EList [ str "d"; len; true_; Lit (Bool false); Lit (Bool false) ])
            SFVL.empty,
          None )
  in
  SHeap.set heap al fvl dom (Some (Expr.ALoc alm));
  let meta =
    [
      ("@class", str (cls_name c.cls));
      ("@proto", c.proto);
      ("@extensible", true_);
      ("@sp_lazy", str info.lvar);
      (lazykeys_key, Expr.ESet []);
    ]
    @
    match c.resolver with
    | Some r -> [ ("@sp_resolver", r); ("@sp_open", Expr.Lit (Bool c.open_)) ]
    | None -> []
  in
  let mfvl = List.fold_left (fun ac (k, v) -> SFVL.add (str k) v ac) SFVL.empty meta in
  SHeap.set heap alm mfvl
    (Some (Expr.ESet (List.sort_uniq Expr.compare (List.map (fun (k, _) -> str k) meta))))
    (Some null)

type branch = SHeap.t * Expr.t list * (string * Type.t) list * string

let materialize (ms : mstate) (info : info) : branch list =
  let x = Expr.LVar info.lvar in
  let n = Array.length info.classes in
  if n = 0 then
    unsupported
      (Printf.sprintf "memory access to lazy value %s, which cannot be an object"
         info.name);
  let existing =
    List.find_opt
      (fun i ->
        match Hashtbl.find_opt mat_alocs (info.lvar, i) with
        | Some (al, _) -> SHeap.has_loc ms.heap al
        | None -> false)
      (List.init n Fun.id)
  in
  match existing with
  | Some i ->
      let al, _ = Hashtbl.find mat_alocs (info.lvar, i) in
      [ (ms.heap, [ eq x (Expr.ALoc al) ], [], al) ]
  | None ->
      let guards = Array.to_list (Array.map (fun c -> c.guard) info.classes) in
      let all_true = List.for_all (Expr.equal true_) guards in
      let obj_known = Type_env.get ms.gamma info.lvar = Some ObjectType in
      (if not (obj_known && all_true) then
         let not_obj = not_ (type_atom x ObjectType) in
         let resid = if all_true then not_obj else Expr.BinOp (not_obj, Or, not_ (disj guards)) in
         if sat ms [ resid ] then
           unsupported
             (Printf.sprintf
                "memory access to lazy value %s that may not be an object of a \
                 listed class"
                info.name));
      let feasible =
        List.filter
          (fun i ->
            let g = info.classes.(i).guard in
            Expr.equal g true_ || sat ms [ g ])
          (List.init n Fun.id)
      in
      let multi = List.length feasible > 1 in
      List.map
        (fun i ->
          let c = info.classes.(i) in
          let heap = if multi then SHeap.copy ms.heap else ms.heap in
          let al, alm = mat_aloc info i in
          create_object heap info c al alm;
          let lfacts, ltypes =
            match (c.cls, c.resolver, array_len_kind c.members) with
            | Arr_cls, None, Sym ->
                let l = len_var info c in
                let fs = List.filter (fun f -> not (PFS.mem ms.pfs f)) (len_facts c l) in
                let ty =
                  if Type_env.get ms.gamma l = Some NumberType then []
                  else [ (l, Type.NumberType) ]
                in
                (fs, ty)
            | _ -> ([], [])
          in
          let facts =
            (eq x (Expr.ALoc al) :: (if Expr.equal c.guard true_ then [] else [ c.guard ]))
            @ lfacts
          in
          (heap, facts, ltypes, al))
        feasible

(** If [loc] is an unresolved registered lazy value, its materialisation
    branches. *)
let materialize_loc (ms : mstate) (loc : Expr.t) : branch list option =
  let lazy_lvar (e : Expr.t) =
    match e with
    | LVar x when Hashtbl.mem infos x -> Some x
    | _ -> None
  in
  match lazy_lvar loc with
  | None -> (
      match reduce ms loc with
      | LVar x when Hashtbl.mem infos x ->
          if FOSolver.resolve_loc_name ~pfs:ms.pfs ~gamma:ms.gamma loc = None then
            Some (materialize ms (Hashtbl.find infos x))
          else None
      | _ -> None)
  | Some x ->
      if FOSolver.resolve_loc_name ~pfs:ms.pfs ~gamma:ms.gamma loc = None then
        Some (materialize ms (Hashtbl.find infos x))
      else None

(* ------------------------------------------------------------------------ *)
(* Member access on materialised lazy objects (GetCell miss)               *)
(* ------------------------------------------------------------------------ *)

let store_member heap al k (child : info) =
  SHeap.set_fv_pair heap al (str k) (data_desc (Expr.LVar child.lvar));
  set_add heap al lazykeys_key k

let member_access (ms : mstate) (info : info) (i : int) (al : string) (k : string) :
    ret =
  let loc = loc_expr al in
  let prop = str k in
  let none () =
    SHeap.set_fv_pair ms.heap al prop nono;
    [ (ms.heap, [ loc; prop; nono ], [], []) ]
  in
  let c = info.classes.(i) in
  match class_member c k with
  | None -> none ()
  | Some (_, optional) -> (
      let child = get_child ~parent_aloc:al info k in
      let facts, types, restricted = child_facts ms info i k child in
      let v = Expr.LVar child.lvar in
      let fork_on f_in f_out =
        (* [f_in]: member present (cell created); [f_out]: absent (none) *)
        let gamma = gamma_with ms types in
        let ok_in = sat ms ~gamma (f_in :: facts) in
        let ok_out = sat ms ~gamma (f_out :: facts) in
        let copy h = if ok_in && ok_out then SHeap.copy h else h in
        (if ok_in then
           let h = copy ms.heap in
           store_member h al k child;
           [ (h, [ loc; prop; data_desc v ], facts @ [ f_in ], types) ]
         else [])
        @
        if ok_out then (
          let h = copy ms.heap in
          SHeap.set_fv_pair h al prop nono;
          [ (h, [ loc; prop; nono ], facts @ [ f_out ], types) ])
        else []
      in
      match c.cls with
      | Obj_cls when optional && List.mem k object_prototype_names ->
          fork_on (not_ (eq v undef)) (eq v undef)
      | Arr_cls when array_len_kind c.members = Sym ->
          let l = Expr.LVar (len_var info c) in
          let idx = Expr.Lit (Num (float_of_string k)) in
          fork_on (Expr.BinOp (idx, FLessThan, l)) (Expr.BinOp (l, FLessThanEqual, idx))
      | _ ->
          if restricted && not (sat ms ~gamma:(gamma_with ms types) facts) then []
          else (
            store_member ms.heap al k child;
            [ (ms.heap, [ loc; prop; data_desc v ], facts, types) ]))

let is_pristine_obj (heap : SHeap.t) (al : string) : bool =
  string_set heap al written_key = [] && string_set heap al deleted_key = []

let next_elem (x : string) : int =
  let n = 1 + Option.value ~default:0 (Hashtbl.find_opt elem_counter x) in
  Hashtbl.replace elem_counter x n;
  n

(** Read of a symbolic property name on a materialised lazy object. *)
let symbolic_access (ms : mstate) (info : info) (i : int) (al : string)
    (prop : Expr.t) : ret =
  let c = info.classes.(i) in
  match c.cls with
  | Obj_cls -> unsupported "symbolic property name on input object"
  | Arr_cls -> (
      match prop with
      | UnOp (ToStringOp, _) ->
          if not (is_pristine_obj ms.heap al) then
            unsupported "symbolic index into a written input array";
          let items = array_items c.members in
          let e =
            new_info
              ~name:(Printf.sprintf "elem(%s)#%d" info.name (next_elem info.lvar))
              ~kind:"skolem" ~shape:items ~label:(shape_label items)
              ~parent:(Some (info.lvar, None)) ()
          in
          decl_info ~parent_aloc:al e;
          let facts, types = mask_facts ms e in
          let loc = loc_expr al in
          let h1 = SHeap.copy ms.heap and h2 = ms.heap in
          SHeap.set_fv_pair h1 al prop (data_desc (Expr.LVar e.lvar));
          set_meta_cell h1 al symcells_key true_;
          SHeap.set_fv_pair h2 al prop nono;
          set_meta_cell h2 al symcells_key true_;
          [
            (h1, [ loc; prop; data_desc (Expr.LVar e.lvar) ], facts, types);
            (h2, [ loc; prop; nono ], [], []);
          ]
      | _ -> unsupported "symbolic non-index property name on input array")

(** GetCell hook: [prop] is not (syntactically) a field of [al]. [None] when
    [al] is not a lazy object handled by LazyJSON (then Gillian's standard
    behaviour applies). *)
let get_cell_miss (ms : mstate) (al : string) (prop : Expr.t) : ret option =
  match owner_of_aloc al with
  | None -> None
  | Some (x, i) -> (
      let info = Hashtbl.find infos x in
      let c = info.classes.(i) in
      if c.resolver <> None then None
      else
        match reduce ms prop with
        | Lit (String k) -> (
            match cell ms.heap al (str k) with
            | Some ffv -> Some [ (ms.heap, [ loc_expr al; str k; ffv ], [], []) ]
            | None -> Some (member_access ms info i al k))
        | p -> Some (symbolic_access ms info i al p))

(* ------------------------------------------------------------------------ *)
(* Writes (SetCell / DeleteCell hooks)                                      *)
(* ------------------------------------------------------------------------ *)

let before_set_cell (ms : mstate) (al : string) (prop : Expr.t) (value : Expr.t) :
    unit =
  match owner_of_aloc al with
  | None -> ()
  | Some (x, _) ->
      let k =
        match reduce ms prop with
        | Lit (String k) -> k
        | _ -> unsupported "symbolic property name written on input object"
      in
      if meta_cell ms.heap al symcells_key <> None then
        unsupported "write to an input array after a symbolic-index read";
      set_remove ms.heap al lazykeys_key k;
      (match value with
      | Lit Nono ->
          set_remove ms.heap al written_key k;
          set_add ms.heap al deleted_key k
      | _ ->
          set_remove ms.heap al deleted_key k;
          set_add ms.heap al written_key k);
      mark_dirty ms.heap x

(* ------------------------------------------------------------------------ *)
(* Enumeration (GetAllProps hook)                                           *)
(* ------------------------------------------------------------------------ *)

let get_all_props (ms : mstate) (loc : string) :
    (Expr.t list * Expr.t list * (string * Type.t) list) option =
  match owner_of_aloc loc with
  | None ->
      (match meta_cell ms.heap loc "@sp_open" with
      | Some (Lit (Bool false)) | None -> ()
      | Some _ -> unsupported "enumeration of an open object");
      None
  | Some (x, i) -> (
      let info = Hashtbl.find infos x in
      let c = info.classes.(i) in
      if c.cls = Obj_cls || c.resolver <> None then
        unsupported "enumeration of an open object";
      if meta_cell ms.heap loc symcells_key <> None then
        unsupported "enumeration of an input array after a symbolic-index read";
      let n =
        match array_len_kind c.members with
        | Fixed n -> n
        | Sym -> (
            match concrete_of ms (Expr.LVar (len_var info c)) with
            | Some (Lit (Num f)) when Float.is_integer f && f >= 0. && f < 1e7 ->
                int_of_float f
            | _ -> unsupported "enumeration of an input array of symbolic length")
      in
      let facts = ref [] and types = ref [] in
      for idx = 0 to n - 1 do
        let k = string_of_int idx in
        if cell ms.heap loc (str k) = None then (
          let child = get_child ~parent_aloc:loc info k in
          let ms' = { ms with gamma = gamma_with ms !types } in
          let f, t, _ = child_facts ms' info i k child in
          facts := !facts @ f;
          types := !types @ t;
          store_member ms.heap loc k child)
      done;
      match SHeap.ordered_fields ms.heap loc with
      | Ok names -> Some (names, !facts, !types)
      | Error _ -> unsupported "enumeration of an object with a symbolic key")

(* ------------------------------------------------------------------------ *)
(* Registration, prefetched members, queries                               *)
(* ------------------------------------------------------------------------ *)

(** Value of the JS data property [k] of the object at [loc]. *)
let js_prop (heap : SHeap.t) (loc : string) (k : string) : Expr.t option =
  match cell heap loc (str k) with
  | Some (EList (Lit (String "d") :: v :: _)) -> Some v
  | _ -> None

let loc_name_of (ms : mstate) (v : Expr.t) : string option =
  match v with
  | ALoc l | Lit (Loc l) -> Some l
  | _ -> FOSolver.resolve_loc_name ~pfs:ms.pfs ~gamma:ms.gamma v

(** Parse the JS class table (section 3.2 [classes]). *)
let parse_classes (ms : mstate) (shape : J.t) (v : Expr.t) : class_spec list option =
  match v with
  | Lit Undefined | Lit Null -> None
  | _ -> (
      let l =
        match loc_name_of ms v with
        | Some l -> l
        | None -> unsupported "lazy class table is not an array"
      in
      let n =
        match js_prop ms.heap l "length" with
        | Some (Lit (Num f)) when Float.is_integer f && f >= 0. -> int_of_float f
        | _ -> unsupported "lazy class table without a concrete length"
      in
      let from_shape = shape_classes shape in
      let spec idx =
        let el =
          match js_prop ms.heap l (string_of_int idx) with
          | Some e -> (
              match loc_name_of ms e with
              | Some el -> el
              | None -> unsupported "lazy class table entry is not an object")
          | None -> unsupported "lazy class table with a hole"
        in
        let p k = js_prop ms.heap el k in
        let cls =
          match p "cls" with
          | Some (Lit (String "Object")) -> Obj_cls
          | Some (Lit (String "Array")) -> Arr_cls
          | _ -> unsupported "lazy class: cls must be \"Object\" or \"Array\""
        in
        let label =
          match p "label" with
          | Some (Lit (String s)) -> s
          | _ -> cls_name cls
        in
        let proto =
          match p "proto" with
          | Some ((ALoc _ | Lit (Loc _)) as e) -> e
          | Some (Lit Undefined) | Some (Lit Null) | None ->
              if cls = Obj_cls then obj_proto else arr_proto
          | Some e -> (
              match loc_name_of ms e with
              | Some l -> loc_expr l
              | None -> unsupported "lazy class: proto is not an object")
        in
        let resolver =
          match p "resolver" with
          | Some (Lit Undefined) | Some (Lit Null) | None -> None
          | Some e -> (
              match loc_name_of ms e with
              | Some l -> Some (loc_expr l)
              | None -> unsupported "lazy class: resolver is not a function")
        in
        let guard =
          match p "guard" with
          | Some (Lit Undefined) | None -> true_
          | Some g -> g
        in
        let open_ =
          match p "open" with
          | Some (Lit (Bool b)) -> b
          | _ -> false
        in
        let members =
          match List.find_opt (fun c' -> c'.cls = cls) from_shape with
          | Some c' -> c'.members
          | None -> if cls = Obj_cls then json_object_shape else json_array_shape
        in
        { label; cls; proto; resolver; guard; open_; members }
      in
      Some (List.init n spec))

let register (ms : mstate) ~(name : string) ~(shape : string) ~(kind : string)
    ~(classes : Expr.t) ~(parent : (string * string option) option) :
    Expr.t * Expr.t list * (string * Type.t) list =
  let s = shape_of_id shape in
  let classes = parse_classes ms s classes in
  let info =
    new_info ~name ~kind ~shape:s ~label:(`String shape) ?classes ~parent ()
  in
  decl_info info;
  match info.mask with
  | [ BinOp (_, Equal, Lit Undefined) ] -> (undef, [], [])
  | _ ->
      let facts, types = mask_facts ms info in
      (Expr.LVar info.lvar, facts, types)

(** The lazy value [v] denotes: its registry entry and, if it is materialised
    on this path, the object and class. *)
let lazy_of_value (ms : mstate) (v : Expr.t) : (info * (string * int) option) option
    =
  let of_loc l =
    match owner_of_aloc l with
    | Some (x, i) -> Some (Hashtbl.find infos x, Some (l, i))
    | None -> None
  in
  match v with
  | LVar x when Hashtbl.mem infos x -> (
      let info = Hashtbl.find infos x in
      match FOSolver.resolve_loc_name ~pfs:ms.pfs ~gamma:ms.gamma v with
      | Some l -> (
          match owner_of_aloc l with
          | Some (y, i) when y = x -> Some (info, Some (l, i))
          | _ -> Some (info, None))
      | None -> Some (info, None))
  | ALoc l | Lit (Loc l) -> of_loc l
  | _ -> None

let member (ms : mstate) (xv : Expr.t) (kv : Expr.t) :
    Expr.t * Expr.t list * (string * Type.t) list =
  let k =
    match reduce ms kv with
    | Lit (String k) -> k
    | _ -> unsupported "symbolic key in a prefetched member"
  in
  let info, mat =
    match lazy_of_value ms xv with
    | Some p -> p
    | None -> unsupported "prefetched member of a non-lazy value"
  in
  let admitted =
    match mat with
    | Some (al, i) ->
        let c = info.classes.(i) in
        if c.resolver <> None then unsupported "prefetched member of a lazy view";
        if List.mem k (string_set ms.heap al written_key)
           || List.mem k (string_set ms.heap al deleted_key)
        then unsupported "prefetched member of a written key";
        if class_member c k = None then None else Some (Some i)
    | None -> if contributions info k = [] then None else Some None
  in
  match admitted with
  | None -> (undef, [], [])
  | Some ci -> (
      let child = get_child info k in
      match ci with
      | Some i ->
          let f, t, _ = child_facts ms info i k child in
          (Expr.LVar child.lvar, f, t)
      | None ->
          let f, t = mask_facts ms child in
          (Expr.LVar child.lvar, f, t))

let is_lazy (ms : mstate) ?(any = false) (v : Expr.t) : bool =
  match lazy_of_value ms v with
  | None -> false
  | Some (info, _) -> any || not (is_dirty ms.heap info.lvar)

let lazy_name (ms : mstate) (v : Expr.t) : string option =
  Option.map (fun (info, _) -> info.name) (lazy_of_value ms v)

let mark_lazy_key (ms : mstate) ~(loc : string) ~(key : string) : unit =
  if not (SHeap.has_loc ms.heap loc) then
    engine_error ("SERVPIPS mark_lazy_key: unknown location " ^ loc);
  set_add ms.heap loc lazykeys_key key

let define (ms : mstate) ~(loc : string) ~(key : string) (v : Expr.t) : unit =
  raw_set_cell ms.heap loc (str key) (data_desc v);
  mark_lazy_key ms ~loc ~key

let absent (ms : mstate) ~(loc : string) ~(key : string) : unit =
  raw_set_cell ms.heap loc (str key) nono;
  set_remove ms.heap loc lazykeys_key key

(* ------------------------------------------------------------------------ *)
(* Memory action names (implemented by JSILSMemory)                         *)
(* ------------------------------------------------------------------------ *)

let a_lazy = "SpLazy"
let a_member = "SpMember"
let a_is_lazy = "SpIsLazy"
let a_lazy_name = "SpLazyName"
let a_mark_lazy_key = "SpMarkLazyKey"
let a_define = "SpDefine"
let a_absent = "SpAbsent"
let a_serialize = "SpSerialize"

(* Value trees produced by SpSerialize, handed to the caller by id. *)
let serialized : (int, J.t) Hashtbl.t = Hashtbl.create 16
let serialized_counter = ref 0

let stash_serialized (j : J.t) : int =
  incr serialized_counter;
  Hashtbl.replace serialized !serialized_counter j;
  !serialized_counter

let take_serialized (n : int) : J.t option =
  let r = Hashtbl.find_opt serialized n in
  Hashtbl.remove serialized n;
  r

(* ------------------------------------------------------------------------ *)
(* Extern-level wrappers (abstract states)                                  *)
(* ------------------------------------------------------------------------ *)

module Ext = struct
  type ('st, 'vt) env =
    (module ServpipsExterns.ENV with type st = 'st and type vt = 'vt)

  let run (type st vt) (env : (st, vt) env) (action : string) (st : st)
      (args : vt list) : (st * vt list) list =
    let module E = (val env : ServpipsExterns.ENV with type st = st and type vt = vt) in
    if not E.symbolic then
      unsupported ("SERVPIPS LazyJSON needs symbolic execution (" ^ action ^ ")");
    List.map
      (function
        | Ok r -> r
        | Error _ -> engine_error ("SERVPIPS memory action " ^ action ^ " failed"))
      (E.State.execute_action action st args)

  let lit (type st vt) (env : (st, vt) env) (l : Literal.t) : vt =
    let module E = (val env : ServpipsExterns.ENV with type st = st and type vt = vt) in
    E.Val.from_literal l

  let one action = function
    | [ (st, [ v ]) ] -> (st, v)
    | _ -> engine_error ("SERVPIPS memory action " ^ action ^ ": unexpected result")

  let register (type st vt) (env : (st, vt) env) (st : st) ~name ~shape ~kind
      ~(classes : vt) : st * vt =
    one a_lazy
      (run env a_lazy st
         [
           lit env (String name);
           lit env (String shape);
           lit env (String kind);
           classes;
         ])

  let member (type st vt) (env : (st, vt) env) (st : st) (x : vt) (k : vt) :
      st * vt =
    one a_member (run env a_member st [ x; k ])

  let is_lazy (type st vt) (env : (st, vt) env) (st : st) ?(any = false) (v : vt)
      : bool =
    let module E = (val env : ServpipsExterns.ENV with type st = st and type vt = vt) in
    if not E.symbolic then false
    else
      match
        run env a_is_lazy st
          [ v; lit env (String (if any then "any" else "pristine")) ]
      with
      | [ (_, [ b ]) ] -> E.Val.to_literal b = Some (Bool true)
      | _ -> engine_error "SERVPIPS SpIsLazy: unexpected result"

  let lazy_name (type st vt) (env : (st, vt) env) (st : st) (v : vt) :
      string option =
    let module E = (val env : ServpipsExterns.ENV with type st = st and type vt = vt) in
    if not E.symbolic then None
    else
      match run env a_lazy_name st [ v ] with
      | [ (_, [ n ]) ] -> (
          match E.Val.to_literal n with
          | Some (String s) -> Some s
          | _ -> None)
      | _ -> engine_error "SERVPIPS SpLazyName: unexpected result"

  let mark_lazy_key (type st vt) (env : (st, vt) env) (st : st) ~(loc : vt)
      ~(key : vt) : st list =
    List.map fst (run env a_mark_lazy_key st [ loc; key ])

  let define (type st vt) (env : (st, vt) env) (st : st) (o : vt) (k : vt)
      (v : vt) : st list =
    List.map fst (run env a_define st [ o; k; v ])

  let absent (type st vt) (env : (st, vt) env) (st : st) (o : vt) (k : vt) :
      st list =
    List.map fst (run env a_absent st [ o; k ])

  let serialize (type st vt) (env : (st, vt) env) (st : st) (v : vt) : J.t =
    let module E = (val env : ServpipsExterns.ENV with type st = st and type vt = vt) in
    match run env a_serialize st [ v ] with
    | [ (_, [ id ]) ] -> (
        match E.Val.to_literal id with
        | Some (Int z) -> (
            match take_serialized (Z.to_int z) with
            | Some j -> j
            | None -> engine_error "SERVPIPS SpSerialize: lost value tree")
        | _ -> engine_error "SERVPIPS SpSerialize: unexpected result")
    | _ -> engine_error "SERVPIPS SpSerialize: unexpected result"
end

(* ------------------------------------------------------------------------ *)
(* Externs owned by WP2                                                     *)
(* ------------------------------------------------------------------------ *)

let concrete_string (type st vt) (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
    (what : string) (v : vt) : string =
  match E.Val.to_literal v with
  | Some (String s) -> s
  | _ -> unsupported (E.extern ^ ": " ^ what ^ " must be a string literal")

let nth_arg (type st vt) (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
    (args : vt list) (n : int) : vt =
  match List.nth_opt args n with
  | Some v -> v
  | None -> E.Val.from_literal Undefined

(* __servpips_lazy(name, shapeId, kind, classes?) *)
let x_lazy : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env = (module E : ServpipsExterns.ENV with type st = st and type vt = vt) in
        if not (Servpips.enabled ()) then
          unsupported "__servpips_lazy needs --servpips";
        let name = concrete_string env "name" (nth_arg env args 0) in
        let shape = concrete_string env "shapeId" (nth_arg env args 1) in
        let kind = concrete_string env "kind" (nth_arg env args 2) in
        let classes = nth_arg env args 3 in
        let st, v = Ext.register env state ~name ~shape ~kind ~classes in
        [ ServpipsExterns.Return (st, v) ]);
  }

(* __servpips_member(x, key) *)
let x_member : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env = (module E : ServpipsExterns.ENV with type st = st and type vt = vt) in
        let st, v = Ext.member env state (nth_arg env args 0) (nth_arg env args 1) in
        [ ServpipsExterns.Return (st, v) ]);
  }

(* __servpips_is_lazy(v, mode?) with mode "pristine" (default) or "any" *)
let x_is_lazy : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env = (module E : ServpipsExterns.ENV with type st = st and type vt = vt) in
        let any =
          match E.Val.to_literal (nth_arg env args 1) with
          | Some (String "any") -> true
          | Some (String "pristine") | Some Undefined | None -> false
          | _ -> unsupported "__servpips_is_lazy: mode must be \"pristine\" or \"any\""
        in
        let b = Ext.is_lazy env state ~any (nth_arg env args 0) in
        [ ServpipsExterns.Return (state, E.Val.from_literal (Bool b)) ]);
  }

(* __servpips_lazy_name(v): the name of a lazy value, or undefined *)
let x_lazy_name : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env = (module E : ServpipsExterns.ENV with type st = st and type vt = vt) in
        let r =
          match Ext.lazy_name env state (nth_arg env args 0) with
          | Some s -> E.Val.from_literal (String s)
          | None -> E.Val.from_literal Undefined
        in
        [ ServpipsExterns.Return (state, r) ]);
  }

(* __servpips_shapes(jsonText) *)
let x_shapes : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env = (module E : ServpipsExterns.ENV with type st = st and type vt = vt) in
        let text = concrete_string env "the shape table" (nth_arg env args 0) in
        (try set_shapes_text text with
        | Servpips.Path_end _ as e -> raise e
        | e ->
            engine_error
              ("__servpips_shapes: bad shape table: " ^ Printexc.to_string e));
        [ ServpipsExterns.Return (state, E.Val.from_literal Undefined) ]);
  }

let () =
  ServpipsExterns.register "servpips_lazy" x_lazy;
  ServpipsExterns.register "servpips_member" x_member;
  ServpipsExterns.register "servpips_is_lazy" x_is_lazy;
  ServpipsExterns.register "servpips_lazy_name" x_lazy_name;
  ServpipsExterns.register "servpips_shapes" x_shapes

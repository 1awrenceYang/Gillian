(* SERVPIPS value trees (design section 5.1 VT, E17). See ServpipsValue.mli. *)

open Gillian.Gil_syntax
module SL = ServpipsLazy
module Servpips = Gillian.General.Servpips
module J = Yojson.Safe

let max_depth = 32
let max_items = 100_000
let opaque what : J.t = `Assoc [ ("t", `String "opaque"); ("what", `String what) ]
let val_node (e : Expr.t) : J.t = `Assoc [ ("t", `String "val"); ("e", Servpips.expr_json e) ]

let blob_encodings = [ "base64"; "utf8"; "latin1"; "hex"; "utf16le"; "ascii" ]

let lazy_node ~lvar ~aloc ~written ~deleted : J.t =
  `Assoc
    [
      ("t", `String "lazy");
      ("lvar", `String lvar);
      ("aloc", match aloc with Some a -> `String a | None -> `Null);
      ("written", `List (List.map (fun (k, v) -> `List [ `String k; v ]) written));
      ("deleted", `List (List.map (fun k -> `String k) deleted));
    ]

(* Present fields of [loc]: concrete string keys in E15 order, and symbolic
   keys. *)
let split_fields (heap : SHeap.t) (loc : string) : string list * Expr.t list =
  let fvl = SL.fvl_of heap loc in
  let present =
    SFVL.fold
      (fun k v ac ->
        match v with
        | Expr.Lit Nono -> ac
        | _ -> k :: ac)
      fvl []
  in
  let concrete, symbolic =
    List.partition
      (function
        | Expr.Lit (String _) -> true
        | _ -> false)
      present
  in
  let names =
    List.map
      (function
        | Expr.Lit (String s) -> s
        | _ -> assert false)
      concrete
  in
  let idx, named = List.partition SHeap.is_array_index names in
  let cmp_idx a b =
    let c = compare (String.length a) (String.length b) in
    if c <> 0 then c else String.compare a b
  in
  let ord = SHeap.get_ord heap loc in
  let seq k = Option.value ~default:max_int (Expr.Map.find_opt (SL.str k) ord) in
  let cmp_named a b =
    let c = compare (seq a) (seq b) in
    if c <> 0 then c else String.compare a b
  in
  (List.sort cmp_idx idx @ List.sort cmp_named named, symbolic)

exception Opaque of string

let serialize_ms (ms : SL.mstate) (v : Expr.t) : J.t =
  let heap = ms.heap in
  let rec vt depth seen (v : Expr.t) : J.t =
    if depth > max_depth then opaque "depth"
    else
      match classify v with
      | `Lazy_unmat (info : SL.info) -> lazy_unmat depth seen info
      | `Loc l ->
          if List.mem l seen then opaque "cycle"
          else if not (SHeap.has_loc heap l) then opaque "model:missing"
          else (
            match SL.owner_of_aloc l with
            | Some (x, _) -> lazy_obj depth (l :: seen) x l
            | None -> program_obj depth (l :: seen) l)
      | `Val -> val_node v
  and classify (v : Expr.t) =
    match v with
    | ALoc l | Lit (Loc l) -> `Loc l
    | LVar x -> (
        match SL.loc_name_of ms v with
        | Some l -> `Loc l
        | None -> (
            match SL.find x with
            | Some info when SL.may_be_object info -> `Lazy_unmat info
            | _ -> `Val))
    | _ -> `Val
  and value_of_desc depth seen (d : Expr.t) : [ `Skip | `Node of J.t ] =
    match d with
    | EList [ Lit (String "d"); v; _; e; _ ] -> (
        match e with
        | Lit (Bool true) -> `Node (vt (depth + 1) seen v)
        | Lit (Bool false) -> `Skip
        | _ -> raise (Opaque "model:symbolic-attribute"))
    | EList (Lit (String "a") :: _ :: _ :: e :: _) -> (
        match e with
        | Lit (Bool true) -> `Node (opaque "accessor")
        | Lit (Bool false) -> `Skip
        | _ -> raise (Opaque "model:symbolic-attribute"))
    | Lit Nono -> `Skip
    | _ -> `Node (opaque "model:unknown-descriptor")
  and program_obj depth seen (l : string) : J.t =
    let meta k = SL.meta_cell heap l k in
    match meta "@call" with
    | Some _ -> opaque "function"
    | None -> (
        match meta "@sp_kind" with
        | Some (Lit (String "blob")) -> blob l
        | Some (Lit (String ("date" | "stream" | "set" as k))) -> opaque k
        | Some _ -> opaque "model:kind"
        | None -> (
            match meta "@sp_model" with
            | Some (Lit (Bool false)) | None -> (
                match meta "@sp_open" with
                | Some (Lit (Bool false)) | None -> (
                    match meta "@class" with
                    | Some (Lit (String "Array")) -> array depth seen l
                    | _ -> obj depth seen l)
                | Some _ -> opaque "model:open")
            | Some (Lit (String s)) -> opaque ("model:" ^ s)
            | Some _ -> opaque "model:object"))
  and blob (l : string) : J.t =
    let src =
      match SL.js_prop heap l "__sp$src" with
      | None | Some (Lit Null) | Some (Lit Undefined) -> `Null
      | Some e -> Servpips.expr_json e
    in
    let enc =
      match SL.js_prop heap l "__sp$enc" with
      | Some (Lit (String e)) when List.mem e blob_encodings -> `String e
      | _ -> `Null
    in
    `Assoc [ ("t", `String "blob"); ("src", src); ("enc", enc) ]
  and obj depth seen (l : string) : J.t =
    try
      let names, symbolic = split_fields heap l in
      let names =
        List.filter
          (fun k -> not (String.length k >= 5 && String.sub k 0 5 = "__sp$"))
          names
      in
      let props =
        List.filter_map
          (fun k ->
            match SL.cell heap l (SL.str k) with
            | Some d -> (
                match value_of_desc depth seen d with
                | `Skip -> None
                | `Node n -> Some (`List [ `String k; n ]))
            | None -> None)
          names
      in
      let sym =
        List.filter_map
          (fun k ->
            match SL.cell heap l k with
            | Some d -> (
                match value_of_desc depth seen d with
                | `Skip -> None
                | `Node n -> Some (`List [ Servpips.expr_json k; n ]))
            | None -> None)
          symbolic
      in
      `Assoc
        [
          ("t", `String "obj");
          ("aloc", `String l);
          ("props", `List props);
          ("sym", `List sym);
        ]
    with Opaque w -> opaque w
  and array depth seen (l : string) : J.t =
    try
      let len =
        match SL.js_prop heap l "length" with
        | Some e -> SL.reduce ms e
        | None -> raise (Opaque "model:array-without-length")
      in
      match len with
      | Lit (Num f) when Float.is_integer f && f >= 0. ->
          let n = int_of_float f in
          if n > max_items then opaque "depth"
          else
            let items =
              List.init n (fun i ->
                  match SL.cell heap l (SL.str (string_of_int i)) with
                  | Some (EList [ Lit (String "d"); v; _; _; _ ]) ->
                      vt (depth + 1) seen v
                  | Some (EList (Lit (String "a") :: _)) -> opaque "accessor"
                  | Some (Lit Nono) | None -> val_node (Lit Undefined)
                  | Some _ -> opaque "model:unknown-descriptor")
            in
            `Assoc
              [
                ("t", `String "arr");
                ("aloc", `String l);
                ("items", `List items);
                ("len", `Null);
              ]
      | e ->
          `Assoc
            [
              ("t", `String "arr");
              ("aloc", `String l);
              ("items", `List []);
              ("len", Servpips.expr_json e);
            ]
    with Opaque w -> opaque w
  (* Lazily created members whose value was written on this path (their
     tree carries the nested writes). Children read through a symbolic
     index cannot be placed: the node is then opaque. [cls]: the class of
     the object on this path, if materialised. *)
  and dirty_children depth seen (x : string) ~(skip : string list)
      ~(cls : SL.class_spec option) : (string * J.t) list =
    List.filter_map
      (fun (k, c) ->
        if not (SL.is_dirty heap c) then None
        else
          match k with
          | None -> raise (Opaque "model:symbolic-index-write")
          | Some k when List.mem k skip -> None
          | Some k -> (
              (match cls with
              | Some cls when SL.class_struct_member cls k = None ->
                  raise (Opaque "model:inconsistent-member")
              | _ -> ());
              Some (k, vt (depth + 1) seen (Expr.LVar c))))
      (SL.children_list x)
  and lazy_unmat depth seen (info : SL.info) : J.t =
    try
      let written =
        if SL.is_dirty heap info.lvar then
          dirty_children depth seen info.lvar ~skip:[] ~cls:None
        else []
      in
      lazy_node ~lvar:info.lvar ~aloc:None ~written ~deleted:[]
    with Opaque w -> opaque w
  and lazy_obj depth seen (x : string) (l : string) : J.t =
    try
      let set k = SL.string_set heap l k in
      let written_keys = set SL.written_key in
      let deleted = set SL.deleted_key in
      let order, _ = split_fields heap l in
      let ordered keys = List.filter (fun k -> List.mem k keys) order in
      let cls =
        match SL.owner_of_aloc l with
        | Some (x', i) when x' = x -> (
            match SL.find x with
            | Some info -> Some info.classes.(i)
            | None -> None)
        | _ -> None
      in
      (* A view (class with a resolver): the keys its resolver defined
         with a value other than the input's own member (e.g. a Buffer
         wrapping the string member Body of an S3 response) are reported as
         written, so that the tree denotes the object the program sees. A
         defined value is the input's own member when it is the lazy value
         named [memberPath(<name>, k)], or [undefined] for a key whose
         existence follows its value (@sp_lazykeys). *)
      let defined_keys =
        match (cls, SL.find x) with
        | Some { SL.resolver = Some _; _ }, Some info ->
            let lazykeys = set SL.lazykeys_key in
            let own_member k (v : Expr.t) =
              match v with
              | LVar c -> (
                  match SL.find c with
                  | Some ci -> ci.name = SL.member_path info.name k
                  | None -> false)
              | Lit Undefined -> List.mem k lazykeys
              | _ -> false
            in
            List.filter
              (fun k ->
                (not (List.mem k written_keys))
                && (not (List.mem k deleted))
                &&
                match SL.cell heap l (SL.str k) with
                | Some (EList [ Lit (String "d"); v; _; _; _ ]) ->
                    not (own_member k (SL.reduce ms v))
                | Some (Lit Nono) | None -> false
                | Some _ -> true)
              order
        | _ -> []
      in
      let written =
        List.filter_map
          (fun k ->
            match SL.cell heap l (SL.str k) with
            | Some d -> (
                match value_of_desc depth seen d with
                | `Skip -> None
                | `Node n -> Some (k, n))
            | None -> None)
          (ordered (written_keys @ defined_keys))
      in
      (* written but non-enumerable keys are invisible: report them deleted *)
      let hidden =
        List.filter
          (fun k -> not (List.mem_assoc k written))
          (ordered written_keys)
      in
      let dirty =
        if SL.is_dirty heap x then
          dirty_children depth seen x
            ~skip:(written_keys @ deleted @ defined_keys)
            ~cls
        else []
      in
      lazy_node ~lvar:x ~aloc:(Some l) ~written:(written @ dirty)
        ~deleted:(List.sort_uniq String.compare (deleted @ hidden))
    with Opaque w -> opaque w
  in
  vt 0 [] v

let serialize (heap : SHeap.t) pfs gamma (v : Expr.t) : J.t =
  serialize_ms { SL.heap; pfs; gamma } v

(* ------------------------------------------------------------------------ *)
(* Debug extern (not part of I2-SF): __servpips_debug_vt(tag, v, withPc)    *)
(* emits {"ev":"note","code":"vt","msg":tag,"site":null,"data":{"vt":<VT>}}  *)
(* and, when withPc is true, also "pc" and "types" of the current state.    *)
(* Used by the servpips_mem_* regression tests of E17.                       *)
(* ------------------------------------------------------------------------ *)

let x_debug_vt : ServpipsExterns.handler =
  {
    run =
      (fun (type st vt)
           (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        let env =
          (module E : ServpipsExterns.ENV with type st = st and type vt = vt)
        in
        let tag, v, with_pc =
          match args with
          | t :: v :: rest -> (
              ( (match E.Val.to_literal t with
                | Some (String s) -> s
                | _ -> Fmt.to_to_string E.Val.pp t),
                v,
                match rest with
                | p :: _ -> E.Val.to_literal p = Some (Bool true)
                | [] -> false ))
          | [ v ] -> ("", v, false)
          | [] -> ("", E.Val.from_literal Undefined, false)
        in
        let j = SL.Ext.serialize env state v in
        let pc, types =
          if with_pc then
            Servpips.pc_and_types_of_asrt
              (E.State.to_assertions ~to_keep:Containers.SS.empty state)
          else ([], [])
        in
        let data =
          if with_pc then
            `Assoc
              [
                ("vt", j);
                ("pc", Servpips.pc_json pc);
                ("types", Servpips.types_json types);
              ]
          else `Assoc [ ("vt", j) ]
        in
        if Servpips.enabled () then Servpips.note ~code:"vt" ~msg:tag ~data ()
        else
          (* e.g. gillian-js exec: no SERVPIPS log, print the note on stderr *)
          prerr_endline
            (Yojson.Safe.to_string
               (`Assoc
                 [
                   ("ev", `String "note");
                   ("code", `String "vt");
                   ("msg", `String tag);
                   ("site", `Null);
                   ("data", data);
                 ]));
        [ ServpipsExterns.Return (state, E.Val.from_literal Undefined) ]);
  }

let () = ServpipsExterns.register "servpips_debug_vt" x_debug_vt

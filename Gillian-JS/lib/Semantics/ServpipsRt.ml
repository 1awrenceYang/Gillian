(* SERVPIPS externs of the JS compiler / runtime package (WP3).
   See ServpipsRt.mli for the specification of every extern. *)

module Expr = Gillian.Gil_syntax.Expr
module Literal = Gillian.Gil_syntax.Literal
module Type = Gillian.Gil_syntax.Type
module BinOp = Gillian.Gil_syntax.BinOp
module UnOp = Gillian.Gil_syntax.UnOp
module Var = Gillian.Gil_syntax.Var
module Servpips = Gillian.General.Servpips
module X = ServpipsExterns

let path_end status reason = raise (Servpips.Path_end { status; reason })
let register_loc = "$lservpips"

(* ------------------------------------------------------------------------ *)
(* Tables                                                                    *)
(* ------------------------------------------------------------------------ *)

(* Section 4.5 (interface I6). [None] = variadic (>= 2). *)
let builtin_names : (string * int option) list =
  [
    ("str.replace_all", Some 3);
    ("str.contains", Some 2);
    ("str.prefixof", Some 2);
    ("str.suffixof", Some 2);
    ("str.indexof", Some 3);
    ("str.substr", Some 3);
    ("str.from_int", Some 1);
    ("str.to_int", Some 1);
    ("str.in_re.numlit", Some 1);
    ("js.tonumber.isnan", Some 1);
    ("js.tonumber.ispinf", Some 1);
    ("js.tonumber.isninf", Some 1);
    ("js.tonumber.num", Some 1);
    ("decodeURIComponent", Some 1);
    ("decodeURIComponent.ok", Some 1);
    ("String.prototype.toLowerCase", Some 1);
    ("String.prototype.toUpperCase", Some 1);
    ("path.basename", Some 1);
    ("path.dirname", Some 1);
    ("path.extname", Some 1);
    ("path.normalize", Some 1);
    ("Date.prototype.toISOString", Some 1);
    ("JSON.quote", Some 1);
    ("js.num2str", Some 1);
    ("js.toUint32", Some 1);
    (* native GIL operations *)
    ("and", None);
    ("or", None);
    ("not", Some 1);
    ("=", Some 2);
    ("=>", Some 2);
    ("ite", Some 3);
    ("typeof", Some 1);
    ("toNumber", Some 1);
    ("toString", Some 1);
  ]

(* [path.join/<n>] *)
let path_join_arity (name : string) : int option =
  let prefix = "path.join/" in
  if String.starts_with ~prefix name then
    let n_s = String.sub name 10 (String.length name - 10) in
    match int_of_string_opt n_s with
    | Some n when n >= 1 && string_of_int n = n_s -> Some n
    | _ -> None
  else None

let builtin_arity (name : string) : int option option =
  match List.assoc_opt name builtin_names with
  | Some a -> Some a
  | None -> (
      match path_join_arity name with
      | Some n -> Some (Some n)
      | None -> None)

let fresh_types = [ ("Str", Type.StringType); ("Num", NumberType); ("Bool", BooleanType) ]

let decl_kinds =
  [
    "input";
    "env";
    "length";
    "response";
    "error";
    "decision";
    "runtime";
    "skolem";
    "havoc";
  ]

let end_statuses =
  [ "returned"; "threw"; "truncated"; "unknown"; "unsupported"; "error" ]

let outcomes =
  [ "resolved"; "rejected"; "pending"; "init-threw"; "no-handler"; "threw-sync" ]

let phases = [ "init"; "handler"; "after-settle" ]
let mark_flags = [ "model"; "resolver"; "open"; "kind" ]
let kinds = [ "blob"; "date"; "stream"; "set" ]

(* ------------------------------------------------------------------------ *)
(* JSON helpers                                                              *)
(* ------------------------------------------------------------------------ *)

let json_num (f : float) : Yojson.Safe.t =
  if Float.is_integer f && Float.abs f < 9007199254740992. then
    `Int (int_of_float f)
  else `Float f

let json_opt_string = function
  | None -> `Null
  | Some s -> `String s

(* Numbers used by the arithmetic rules *)
let two52 = 4503599627370496.
let two26 = 67108864.

(* Site label of a site string: [file:line:col] (drop the end position). *)
let site_label (site : string) : string =
  let parts = String.split_on_char ':' site in
  let rev = List.rev parts in
  let rec count_ints n = function
    | x :: rest when int_of_string_opt x <> None -> count_ints (n + 1) rest
    | _ -> n
  in
  let n = count_ints 0 rev in
  (* 3 trailing numbers: line:col:endcol; 4: line:col:endline:endcol *)
  let drop = if n >= 4 then 2 else if n = 3 then 1 else 0 in
  let keep = List.length parts - drop in
  String.concat ":" (List.filteri (fun i _ -> i < keep) parts)

let is_pow2_divisor (f : float) : bool =
  Float.is_integer f
  && Float.abs f >= 1.
  &&
  let m, _ = Float.frexp (Float.abs f) in
  m = 0.5

(* ------------------------------------------------------------------------ *)
(* Handlers, over the interpreter's value and state modules                 *)
(* ------------------------------------------------------------------------ *)

module Make (E : X.ENV) = struct
  module V = E.Val
  module S = E.State

  type st = E.st
  type vt = E.vt
  type outcome = (st, vt) X.outcome

  let lit l = V.from_literal l
  let undef = lit Literal.Undefined
  let null = lit Literal.Null
  let vbool b = lit (Literal.Bool b)
  let vstr s = lit (Literal.String s)
  let vnum f = lit (Literal.Num f)
  let e_of (v : vt) : Expr.t = V.to_expr v
  let pp_v v = Fmt.to_to_string V.pp v

  let fail_err fmt =
    Printf.ksprintf (fun m -> path_end "error" (E.extern ^ ": " ^ m)) fmt

  let fail_uns fmt =
    Printf.ksprintf (fun m -> path_end "unsupported" (E.extern ^ ": " ^ m)) fmt

  let v_of_expr (e : Expr.t) : vt =
    match V.from_expr e with
    | Some v -> v
    | None -> fail_uns "cannot represent %s as a value" (Fmt.to_to_string Expr.pp e)

  let string_arg what v =
    match V.to_literal v with
    | Some (String s) -> s
    | _ -> fail_err "%s must be a string literal, got %s" what (pp_v v)

  (* Any engine exception (solver, encoding, typing) ends the path
     unsupported; never a silent drop. *)
  let guard (f : unit -> 'a) : 'a =
    try f () with
    | Servpips.Path_end _ as e -> raise e
    | (Stack_overflow | Out_of_memory) as e -> raise e
    | e -> fail_uns "engine failure: %s" (Printexc.to_string e)

  (* ---- memory ---- *)

  let nono v = V.to_literal v = Some Literal.Nono

  let action1 name (st : st) (args : vt list) : (st * vt list) option =
    let res =
      try S.execute_action name st args with
      | Servpips.Path_end _ as e -> raise e
      | _ -> []
    in
    match res with
    | [ Ok (st', vs) ] -> Some (st', vs)
    | _ -> None

  let get_cell_v st loc (prop : vt) : (st * vt option) option =
    match action1 "GetCell" st [ loc; prop ] with
    | Some (st, [ _; _; v ]) -> Some (st, if nono v then None else Some v)
    | _ -> None

  let get_cell st loc (prop : string) = get_cell_v st loc (vstr prop)

  let set_cell st loc (prop : vt) (v : vt) : st =
    match action1 "SetCell" st [ loc; prop; v ] with
    | Some (st, _) -> st
    | None -> fail_err "cannot write %s.%s" (pp_v loc) (pp_v prop)

  let get_metadata st loc : (st * vt) option =
    match action1 "GetMetadata" st [ loc ] with
    | Some (st, [ _; m ]) when not (nono m) -> Some (st, m)
    | _ -> None

  let get_all_props st loc : (st * vt list) option =
    match action1 "GetAllProps" st [ loc ] with
    | Some (st, [ _; props ]) -> (
        match V.to_list props with
        | Some l -> Some (st, l)
        | None -> None)
    | _ -> None

  let is_loc (v : vt) =
    match e_of v with
    | Expr.Lit (Loc _) | ALoc _ -> true
    | _ -> false

  let loc_name (v : vt) =
    match e_of v with
    | Expr.Lit (Loc l) | ALoc l -> l
    | e -> Fmt.to_to_string Expr.pp e

  type prop = Absent | Data of vt * bool | Accessor of bool | Unknown

  let enumerable_flag e = V.to_literal e <> Some (Literal.Bool false)

  let own_prop_v st obj (p : vt) : st * prop =
    match get_cell_v st obj p with
    | None -> (st, Unknown)
    | Some (st, None) -> (st, Absent)
    | Some (st, Some d) -> (
        match V.to_list d with
        | Some (tag :: v :: _ :: e :: _)
          when V.to_literal tag = Some (String "d") ->
            (st, Data (v, enumerable_flag e))
        | Some (tag :: _ :: _ :: e :: _)
          when V.to_literal tag = Some (String "a") ->
            (st, Accessor (enumerable_flag e))
        | _ -> (st, Unknown))

  let own_prop st obj (p : string) = own_prop_v st obj (vstr p)

  (* metadata field of a JS object *)
  let meta_field st obj (f : string) : st * vt option =
    match get_metadata st obj with
    | None -> (st, None)
    | Some (st, m) -> (
        match get_cell st m f with
        | Some (st, v) -> (st, v)
        | None -> (st, None))

  (* ---- register ---- *)

  let reg = lit (Literal.Loc register_loc)

  let reg_get st f : (st * vt) option =
    match get_cell st reg f with
    | Some (st, Some v) -> Some (st, v)
    | _ -> None

  let reg_set st f v : st option =
    match action1 "SetCell" st [ reg; vstr f; v ] with
    | Some (st, _) -> Some st
    | None -> None

  (* per-path counter [k:<name>] of the register, 1-based *)
  let next_count st (name : string) : st * int =
    let key = "k:" ^ name in
    let prev =
      match reg_get st key with
      | Some (_, v) -> (
          match V.to_literal v with
          | Some (Num f) -> int_of_float f
          | _ -> 0)
      | None -> 0
    in
    let k = prev + 1 in
    match reg_set st key (vnum (float_of_int k)) with
    | Some st -> (st, k)
    | None -> fail_err "internal register object %s missing" register_loc

  (* ---- path condition ---- *)

  let pc_types st =
    try
      Servpips.pc_and_types_of_asrt
        (S.to_assertions ~to_keep:Containers.SS.empty st)
    with _ -> ([], [])

  (* [assume_all st fs]: copy of [st] with every formula of [fs] assumed, or
     [None] if unsatisfiable. *)
  let assume_all (st : st) (fs : Expr.t list) : st option =
    guard (fun () ->
        List.fold_left
          (fun acc f ->
            match acc with
            | None -> None
            | Some st -> (
                match Expr.to_literal f with
                | Some (Bool true) -> Some st
                | _ -> (
                    match S.assume st (v_of_expr f) with
                    | st' :: _ -> Some st'
                    | [] -> None)))
          (Some (S.copy st)) fs)

  let sat (st : st) (fs : Expr.t list) : bool =
    guard (fun () ->
        match fs with
        | [] -> true
        | _ -> S.sat_check st (v_of_expr (Expr.conjunct fs)))

  let entails (st : st) (fs : Expr.t list) : bool =
    guard (fun () -> S.assert_a st fs)

  (* a fresh spec logical variable of type [ty] *)
  let fresh_var st (ty : Type.t) : st * vt * string =
    let x = Generators.fresh_svar () in
    let st = S.add_spec_vars st (Var.Set.singleton x) in
    let v = V.from_lvar_name x in
    match guard (fun () -> S.assume_t st v ty) with
    | Some st -> (st, v, x)
    | None -> fail_err "cannot give type %s to a fresh variable" (Type.str ty)

  let emit_decl ~lvar ~name ~sort ~kind ?(parent = `Null) ?(key = `Null)
      ?(parent_lvar = `Null) ?(parent_aloc = `Null) ?(shape = `Null)
      ?(site = `Null) ?(k = `Null) () =
    Servpips.emit
      (`Assoc
        [
          ("ev", `String "decl");
          ("lvar", `String lvar);
          ("name", `String name);
          ("sort", `String sort);
          ("kind", `String kind);
          ("parent", parent);
          ("key", key);
          ("parent_lvar", parent_lvar);
          ("parent_aloc", parent_aloc);
          ("shape", shape);
          ("open", `Null);
          ("site", site);
          ("k", k);
        ])

  (* ---- value trees (fallback serialiser; E17 belongs to WP2) ---- *)

  let json_of_scalar (v : vt) : Yojson.Safe.t =
    match V.to_literal v with
    | Some (String s) -> `String s
    | Some (Num f) -> json_num f
    | Some (Bool b) -> `Bool b
    | Some Null -> `Null
    | _ -> `String (pp_v v)

  let index_key (s : string) : int option =
    match int_of_string_opt s with
    | Some i when i >= 0 && string_of_int i = s -> Some i
    | _ -> None

  let rec vt_of (st : st) (depth : int) (seen : string list) (v : vt) :
      st * Yojson.Safe.t =
    let e = e_of v in
    let val_node () = (st, `Assoc [ ("t", `String "val"); ("e", Servpips.expr_json e) ]) in
    let opaque what = (st, `Assoc [ ("t", `String "opaque"); ("what", `String what) ]) in
    if not (is_loc v) then val_node ()
    else if depth > 32 then opaque "depth"
    else
      let name = loc_name v in
      if List.mem name seen then opaque "cycle"
      else
        match get_metadata st v with
        | None -> val_node ()
        | Some (st, m) -> (
            let field st f =
              match get_cell st m f with
              | Some (st, x) -> (st, x)
              | None -> (st, None)
            in
            let st, call = field st "@call" in
            let st, kind = field st "@sp_kind" in
            let st, lazyv = field st "@sp_lazy" in
            let st, model = field st "@sp_model" in
            let st, cls = field st "@class" in
            let cls = match Option.bind cls V.to_literal with Some (String c) -> c | _ -> "Object" in
            let is_true o = match o with None -> false | Some x -> V.to_literal x <> Some (Bool false) in
            let seen = name :: seen in
            match kind with
            | _ when call <> None -> opaque "function"
            | Some k when V.to_literal k = Some (String "blob") ->
                let st, src = own_prop st v "__sp$src" in
                let st, enc = own_prop st v "__sp$enc" in
                let src =
                  match src with
                  | Data (s, _) when V.to_literal s <> Some Null -> Servpips.expr_json (e_of s)
                  | _ -> `Null
                in
                let enc =
                  match enc with
                  | Data (s, _) -> (match V.to_literal s with Some (String s) -> `String s | _ -> `Null)
                  | _ -> `Null
                in
                (st, `Assoc [ ("t", `String "blob"); ("src", src); ("enc", enc) ])
            | Some k -> (
                match V.to_literal k with
                | Some (String ("date" | "stream" | "set" as w)) -> opaque w
                | _ -> opaque "model:unknown")
            | None when lazyv <> None ->
                (* LazyJSON: WP2's serialiser gives the lazy node *)
                opaque "lazy"
            | None when is_true model -> opaque ("model:" ^ cls)
            | None when cls = "Array" -> (
                let st, len = own_prop st v "length" in
                match len with
                | Data (l, _) -> (
                    match V.to_literal l with
                    | Some (Num n) when Float.is_integer n && n >= 0. && n <= 100000. ->
                        let rec items st i acc =
                          if i >= int_of_float n then (st, List.rev acc)
                          else
                            let st, p = own_prop st v (string_of_int i) in
                            let st, j =
                              match p with
                              | Data (x, _) -> vt_of st (depth + 1) seen x
                              | Accessor _ -> (st, `Assoc [ ("t", `String "opaque"); ("what", `String "accessor") ])
                              | Absent -> (st, `Assoc [ ("t", `String "val"); ("e", Servpips.expr_json (Lit Undefined)) ])
                              | Unknown -> (st, `Assoc [ ("t", `String "opaque"); ("what", `String "unknown") ])
                            in
                            items st (i + 1) (j :: acc)
                        in
                        let st, its = items st 0 [] in
                        (st, `Assoc [ ("t", `String "arr"); ("aloc", `String name); ("items", `List its); ("len", `Null) ])
                    | _ ->
                        (st, `Assoc [ ("t", `String "arr"); ("aloc", `String name); ("items", `List []); ("len", Servpips.expr_json (e_of l)) ]))
                | _ -> opaque "array")
            | None -> (
                match get_all_props st v with
                | None -> opaque "unknown"
                | Some (st, props) ->
                    let named, symbolic =
                      List.fold_left
                        (fun (n, s) p ->
                          match V.to_literal p with
                          | Some (String k) when String.length k > 0 && k.[0] = '@' -> (n, s)
                          | Some (String k) -> ((k, p) :: n, s)
                          | _ -> (n, p :: s))
                        ([], []) props
                    in
                    let named = List.rev named and symbolic = List.rev symbolic in
                    (* ES2020 OrdinaryOwnPropertyKeys: integer keys ascending,
                       then the others (heap order; creation order is WP2's) *)
                    let ints, others =
                      List.partition (fun (k, _) -> index_key k <> None) named
                    in
                    let ints =
                      List.sort (fun (a, _) (b, _) -> compare (index_key a) (index_key b)) ints
                    in
                    let st, props_j =
                      List.fold_left
                        (fun (st, acc) (k, p) ->
                          let st, pr = own_prop_v st v p in
                          match pr with
                          | Data (x, true) ->
                              let st, j = vt_of st (depth + 1) seen x in
                              (st, `List [ `String k; j ] :: acc)
                          | Accessor true ->
                              (st, `List [ `String k; `Assoc [ ("t", `String "opaque"); ("what", `String "accessor") ] ] :: acc)
                          | Unknown ->
                              (st, `List [ `String k; `Assoc [ ("t", `String "opaque"); ("what", `String "unknown") ] ] :: acc)
                          | _ -> (st, acc))
                        (st, []) (ints @ others)
                    in
                    let st, sym_j =
                      List.fold_left
                        (fun (st, acc) p ->
                          let st, pr = own_prop_v st v p in
                          match pr with
                          | Data (x, true) ->
                              let st, j = vt_of st (depth + 1) seen x in
                              (st, `List [ Servpips.expr_json (e_of p); j ] :: acc)
                          | Absent | Data (_, false) | Accessor false -> (st, acc)
                          | _ ->
                              (st, `List [ Servpips.expr_json (e_of p); `Assoc [ ("t", `String "opaque"); ("what", `String "unknown") ] ] :: acc))
                        (st, []) symbolic
                    in
                    ( st,
                      `Assoc
                        [
                          ("t", `String "obj");
                          ("aloc", `String name);
                          ("props", `List (List.rev props_j));
                          ("sym", `List (List.rev sym_j));
                        ] )))

  let serialize (st : st) (v : vt) : st * Yojson.Safe.t =
    let via_memory =
      match
        try S.execute_action "ServpipsSerialize" st [ v ] with
        | Servpips.Path_end _ as e -> raise e
        | _ -> []
      with
      | [ Ok (st', [ j ]) ] -> (
          match V.to_literal j with
          | Some (String text) -> (
              try Some (st', Yojson.Safe.from_string text) with _ -> None)
          | _ -> None)
      | _ -> None
    in
    match via_memory with
    | Some r -> r
    | None -> vt_of st 0 [] v

  (* ------------------------------------------------------------------ *)
  (* servpips_site(fn)                                                   *)
  (* ------------------------------------------------------------------ *)

  let site (st : st) (args : vt list) : outcome list =
    let fn =
      match args with
      | fn :: _ -> fn
      | [] -> undef
    in
    match reg_get st "callee" with
    | None -> [ X.Return (st, null) ]
    | Some (st, callee) -> (
        if V.to_literal callee = Some Null then [ X.Return (st, null) ]
        else
          (* the callee and its bound targets *)
          let rec targets st v depth acc =
            if depth > 8 || not (is_loc v) then (st, List.rev acc)
            else
              let st, tf = meta_field st v "@targetFunction" in
              match tf with
              | Some tf -> targets st tf (depth + 1) (tf :: acc)
              | None -> (st, List.rev acc)
          in
          let st, bound = targets st callee 0 [] in
          let cands = callee :: bound in
          let eqs =
            List.map (fun c -> Expr.BinOp (e_of c, Equal, e_of fn)) cands
          in
          let f_match = Expr.disjunct eqs in
          let consume st =
            let site =
              match reg_get st "site" with
              | Some (_, s) -> s
              | None -> null
            in
            match reg_set st "callee" null with
            | Some st -> X.Return (st, site)
            | None -> fail_err "internal register object missing"
          in
          if not E.symbolic then
            if List.exists (fun c -> V.equal c fn) cands then [ consume st ]
            else [ X.Return (st, null) ]
          else if entails st [ f_match ] then [ consume st ]
          else if entails st [ Expr.UnOp (Not, f_match) ] then
            [ X.Return (st, null) ]
          else
            let yes =
              match assume_all st [ f_match ] with
              | Some st -> [ consume st ]
              | None -> []
            in
            let no =
              match assume_all st [ Expr.UnOp (Not, f_match) ] with
              | Some st -> [ X.Return (st, null) ]
              | None -> []
            in
            yes @ no)

  (* ------------------------------------------------------------------ *)
  (* servpips_emit(kind, ...)                                            *)
  (* ------------------------------------------------------------------ *)

  let info_string st info f ~required =
    let st, p = own_prop st info f in
    match p with
    | Data (v, _) -> (
        match V.to_literal v with
        | Some (String s) -> (st, Some s)
        | Some Undefined when not required -> (st, None)
        | _ -> fail_err "call info field %s must be a string, got %s" f (pp_v v))
    | Absent when not required -> (st, None)
    | _ -> fail_err "call info field %s missing" f

  let emit_call st info params : outcome list =
    if not (is_loc info) then fail_err "call info must be an object";
    let st, site = info_string st info "site" ~required:true in
    let st, callee = info_string st info "callee" ~required:false in
    let st, api = info_string st info "api" ~required:true in
    let st, sdk = info_string st info "sdk" ~required:false in
    let st, phase = info_string st info "phase" ~required:false in
    let phase = Option.value ~default:"handler" phase in
    if not (List.mem phase phases) then fail_err "invalid phase %s" phase;
    let st, k =
      let st, p = own_prop st info "k" in
      match p with
      | Data (v, _) -> (
          match V.to_literal v with
          | Some (Num f) -> (st, json_num f)
          | Some Undefined | Some Null -> (st, `Null)
          | _ -> fail_uns "call info field k is not concrete: %s" (pp_v v))
      | _ -> (st, `Null)
    in
    let st, sent =
      let st, p = own_prop st info "sent" in
      match p with
      | Data (v, _) -> (
          match V.to_literal v with
          | Some (Bool b) -> (st, b)
          | _ -> fail_uns "call info field sent is not a boolean literal: %s" (pp_v v))
      | Absent -> (st, true)
      | _ -> fail_err "call info field sent unreadable"
    in
    let st, params_j = guard (fun () -> serialize st params) in
    let pc, types = pc_types st in
    Servpips.emit
      (`Assoc
        [
          ("ev", `String "call");
          ("site", json_opt_string site);
          ("callee", json_opt_string callee);
          ("api", json_opt_string api);
          ("sdk", json_opt_string sdk);
          ("k", k);
          ("sent", `Bool sent);
          ("phase", `String phase);
          ("params", params_j);
          ("pc", Servpips.pc_json pc);
          ("types", Servpips.types_json types);
        ]);
    [ X.Return (st, undef) ]

  let emit (st : st) (args : vt list) : outcome list =
    match args with
    | [] -> fail_err "missing event kind"
    | kind :: rest -> (
        match (string_arg "event kind" kind, rest) with
        | "call", info :: params :: _ -> emit_call st info params
        | "call", [ info ] -> emit_call st info undef
        | "note", code :: rest ->
            let code = string_arg "note code" code in
            let msg, data =
              match rest with
              | [] -> ("", None)
              | [ m ] -> (
                  ( (match V.to_literal m with
                    | Some (String s) -> s
                    | _ -> pp_v m),
                    None ))
              | m :: d :: _ ->
                  ( (match V.to_literal m with
                    | Some (String s) -> s
                    | _ -> pp_v m),
                    Some d )
            in
            let st, data =
              match data with
              | None -> (st, None)
              | Some d when V.to_literal d = Some Undefined -> (st, None)
              | Some d ->
                  let st, j = guard (fun () -> serialize st d) in
                  (st, Some j)
            in
            Servpips.note ~code ~msg ?data ();
            [ X.Return (st, undef) ]
        | "end", status :: rest ->
            let status = string_arg "end status" status in
            let reason =
              match rest with
              | r :: _ -> (
                  match V.to_literal r with
                  | Some (String s) -> s
                  | _ -> pp_v r)
              | [] -> ""
            in
            if not (List.mem status end_statuses) then
              fail_err "invalid end status %s (reason %s)" status reason;
            path_end status reason
        | "outcome", o :: _ ->
            let o = string_arg "outcome" o in
            if not (List.mem o outcomes) then fail_err "invalid outcome %s" o;
            let st =
              match reg_set st "outcome" (vstr o) with
              | Some st -> st
              | None -> st
            in
            Servpips.note ~code:"outcome" ~msg:o ();
            [ X.Return (st, undef) ]
        | k, _ -> fail_err "invalid event %s or missing arguments" k)

  (* ------------------------------------------------------------------ *)
  (* servpips_fresh(name, type, kind [, meta])                           *)
  (* ------------------------------------------------------------------ *)

  let fresh (st : st) (args : vt list) : outcome list =
    match args with
    | name :: ty :: kind :: rest ->
        let name = string_arg "name" name in
        let ty_s = string_arg "type" ty in
        let kind = string_arg "kind" kind in
        let ty =
          match List.assoc_opt ty_s fresh_types with
          | Some t -> t
          | None -> fail_err "invalid type %s (Str|Num|Bool)" ty_s
        in
        if not (List.mem kind decl_kinds) then fail_err "invalid kind %s" kind;
        if not E.symbolic then fail_uns "fresh %s under concrete execution" name;
        let st, meta =
          match rest with
          | m :: _ when is_loc m ->
              let get st f =
                let st, p = own_prop st m f in
                match p with
                | Data (v, _) -> (
                    match V.to_literal v with
                    | Some Undefined -> (st, `Null)
                    | Some _ -> (st, json_of_scalar v)
                    | None -> fail_err "meta field %s is not concrete" f)
                | _ -> (st, `Null)
              in
              let st, site = get st "site" in
              let st, k = get st "k" in
              let st, parent = get st "parent" in
              let st, key = get st "key" in
              let st, parent_lvar = get st "parent_lvar" in
              let st, parent_aloc = get st "parent_aloc" in
              let st, shape = get st "shape" in
              (st, [ site; k; parent; key; parent_lvar; parent_aloc; shape ])
          | _ -> (st, [ `Null; `Null; `Null; `Null; `Null; `Null; `Null ])
        in
        let st, v, x = fresh_var st ty in
        (match meta with
        | [ site; k; parent; key; parent_lvar; parent_aloc; shape ] ->
            emit_decl ~lvar:x ~name ~sort:ty_s ~kind ~parent ~key ~parent_lvar
              ~parent_aloc ~shape ~site ~k ()
        | _ -> ());
        [ X.Return (st, v) ]
    | _ -> fail_err "expected (name, type, kind [, meta])"

  (* ------------------------------------------------------------------ *)
  (* servpips_assume(b)                                                  *)
  (* ------------------------------------------------------------------ *)

  let assume (st : st) (args : vt list) : outcome list =
    let b =
      match args with
      | b :: _ -> b
      | [] -> fail_err "missing argument"
    in
    let e = e_of b in
    match Expr.to_literal e with
    | Some (Bool true) -> [ X.Return (st, undef) ]
    | Some (Bool false) -> []
    | Some _ -> fail_err "argument is not a boolean: %s" (pp_v b)
    | None ->
        if not E.symbolic then fail_err "non-literal assume under concrete execution";
        let is_bool =
          match guard (fun () -> S.get_type st b) with
          | Some BooleanType -> true
          | _ -> (
              match e with
              | Expr.LVar _ | PVar _ -> false
              | _ -> Expr.is_boolean_expr e)
        in
        if not is_bool then
          fail_err "argument is not known to be a GIL boolean: %s" (pp_v b);
        (match assume_all st [ e ] with
        | Some st -> [ X.Return (st, undef) ]
        | None -> [])

  (* ------------------------------------------------------------------ *)
  (* servpips_fn("<name>", a1, ...)                                      *)
  (* ------------------------------------------------------------------ *)

  let fn (st : st) (args : vt list) : outcome list =
    match args with
    | [] -> fail_err "missing builtin name"
    | name :: fargs -> (
        let name = string_arg "builtin name" name in
        let n = List.length fargs in
        (match builtin_arity name with
        | None -> fail_err "unknown builtin %s" name
        | Some None -> if n < 2 then fail_err "%s expects at least 2 arguments" name
        | Some (Some a) ->
            if n <> a then fail_err "%s expects %d arguments, got %d" name a n);
        let es = List.map e_of fargs in
        let native : Expr.t option =
          match (name, es) with
          | "and", e :: rest ->
              Some (List.fold_left (fun acc x -> Expr.BinOp (acc, And, x)) e rest)
          | "or", e :: rest ->
              Some (List.fold_left (fun acc x -> Expr.BinOp (acc, Or, x)) e rest)
          | "not", [ e ] -> Some (UnOp (Not, e))
          | "=", [ a; b ] -> Some (BinOp (a, Equal, b))
          | "=>", [ a; b ] -> Some (BinOp (a, Impl, b))
          | "ite", [ c; a; b ]
            when Expr.is_boolean_expr a && Expr.is_boolean_expr b ->
              Some
                (BinOp
                   (BinOp (c, And, a), Or, BinOp (UnOp (Not, c), And, b)))
          | "typeof", [ e ] -> Some (UnOp (TypeOf, e))
          | "toNumber", [ e ] -> Some (UnOp (ToNumberOp, e))
          | "toString", [ e ] -> Some (UnOp (ToStringOp, e))
          | _ -> None
        in
        match native with
        | Some e -> [ X.Return (st, guard (fun () -> S.eval_expr st e)) ]
        | None ->
            let e = Expr.FuncApp (name, es) in
            if not E.symbolic then
              fail_uns "builtin %s under concrete execution" name
            else [ X.Return (st, v_of_expr e) ])

  (* ------------------------------------------------------------------ *)
  (* servpips_define / servpips_absent / servpips_mark                   *)
  (* ------------------------------------------------------------------ *)

  let lazykeys_update st obj (k : string) ~(add : bool) : st =
    match get_metadata st obj with
    | None -> fail_err "object %s has no metadata" (pp_v obj)
    | Some (st, m) -> (
        let st, cur =
          match get_cell st m "@sp_lazykeys" with
          | Some (st, v) -> (st, v)
          | None -> (st, None)
        in
        let set v = set_cell st m (vstr "@sp_lazykeys") v in
        match cur with
        | None -> if add then set (lit (LList [ String k ])) else st
        | Some cur -> (
            match V.to_literal cur with
            | Some (LList l) ->
                let l' = List.filter (fun x -> x <> Literal.String k) l in
                let l' = if add then l' @ [ Literal.String k ] else l' in
                set (lit (LList l'))
            | _ -> (
                match e_of cur with
                | Expr.ESet es ->
                    let es' = List.filter (fun x -> x <> Expr.Lit (String k)) es in
                    let es' = if add then es' @ [ Expr.Lit (String k) ] else es' in
                    set (v_of_expr (ESet es'))
                | Expr.EList es ->
                    let es' = List.filter (fun x -> x <> Expr.Lit (String k)) es in
                    let es' = if add then es' @ [ Expr.Lit (String k) ] else es' in
                    set (v_of_expr (EList es'))
                | _ ->
                    fail_uns "unexpected @sp_lazykeys representation %s"
                      (pp_v cur))))

  let obj_key_args what args =
    match args with
    | o :: k :: rest ->
        if not (is_loc o) then fail_err "%s: not an object: %s" what (pp_v o);
        let k =
          match V.to_literal k with
          | Some (String s) -> s
          | _ -> fail_uns "%s: symbolic or non-string property name %s" what (pp_v k)
        in
        (o, k, rest)
    | _ -> fail_err "%s: expected (object, key, ...)" what

  let define (st : st) (args : vt list) : outcome list =
    let o, k, rest = obj_key_args "define" args in
    let v =
      match rest with
      | v :: _ -> v
      | [] -> undef
    in
    let desc = V.from_list [ vstr "d"; v; vbool true; vbool true; vbool true ] in
    let st = set_cell st o (vstr k) desc in
    let st = lazykeys_update st o k ~add:true in
    [ X.Return (st, undef) ]

  let absent (st : st) (args : vt list) : outcome list =
    let o, k, _ = obj_key_args "absent" args in
    let st = set_cell st o (vstr k) (lit Nono) in
    let st = lazykeys_update st o k ~add:false in
    [ X.Return (st, undef) ]

  let mark (st : st) (args : vt list) : outcome list =
    match args with
    | o :: flag :: rest -> (
        if not (is_loc o) then fail_err "not an object: %s" (pp_v o);
        let flag = string_arg "flag" flag in
        if not (List.mem flag mark_flags) then fail_err "invalid flag %s" flag;
        let value =
          match rest with
          | v :: _ -> v
          | [] -> vbool true
        in
        (match (flag, V.to_literal value) with
        | ("model" | "open"), Some (Bool _) -> ()
        | "kind", Some (String k) when List.mem k kinds -> ()
        | "resolver", _ when is_loc value -> ()
        | _ -> fail_err "invalid value %s for flag %s" (pp_v value) flag);
        match get_metadata st o with
        | None -> fail_err "object %s has no metadata" (pp_v o)
        | Some (st, m) ->
            let st = set_cell st m (vstr ("@sp_" ^ flag)) value in
            [ X.Return (st, undef) ])
    | _ -> fail_err "expected (object, flag, value)"

  let is_concrete (st : st) (args : vt list) : outcome list =
    let v =
      match args with
      | v :: _ -> v
      | [] -> undef
    in
    let v = if E.symbolic then (try S.simplify_val st v with _ -> v) else v in
    let b =
      match V.to_literal v with
      | Some _ -> true
      | None -> false
    in
    [ X.Return (st, vbool b) ]

  let rejected (st : st) (args : vt list) : outcome list =
    if Servpips.enabled () then
      let reason =
        match args with
        | r :: _ -> (
            match V.to_literal r with
            | Some (String s) -> s
            | _ -> pp_v r)
        | [] -> "rejected"
      in
      path_end "unsupported" reason
    else [ X.Return (st, undef) ]

  (* ------------------------------------------------------------------ *)
  (* servpips_tonumber(s)  (E18, design section 4.4)                     *)
  (* ------------------------------------------------------------------ *)

  let tonumber (st : st) (args : vt list) : outcome list =
    let s =
      match args with
      | s :: _ -> s
      | [] -> fail_err "missing argument"
    in
    let es = e_of s in
    let plain () =
      [ X.Return (st, guard (fun () -> S.eval_expr st (UnOp (ToNumberOp, es)))) ]
    in
    match V.to_literal s with
    | Some _ -> plain ()
    | None when (not (Servpips.enabled ())) || not E.symbolic -> plain ()
    | None ->
        let app f = Expr.FuncApp (f, [ es ]) in
        let numlit = app "str.in_re.numlit" in
        let isnan = app "js.tonumber.isnan" in
        let ispinf = app "js.tonumber.ispinf" in
        let isninf = app "js.tonumber.isninf" in
        let not_ e = Expr.UnOp (Not, e) in
        let eq a b = Expr.BinOp (a, Equal, b) in
        let sl s = Expr.Lit (String s) in
        let digits =
          (* s is 1..15 decimal digits: str.to_int s >= 0 and 1 <= |s| <= 15 *)
          Expr.BinOp
            ( BinOp (Lit (Num 0.), FLessThanEqual, app "str.to_int"),
              And,
              BinOp
                ( BinOp (Lit (Num 1.), FLessThanEqual, UnOp (StrLen, es)),
                  And,
                  BinOp (UnOp (StrLen, es), FLessThanEqual, Lit (Num 15.)) ) )
        in
        let axioms =
          [
            eq numlit (not_ isnan);
            not_ (BinOp (ispinf, And, isninf));
            BinOp (ispinf, Impl, numlit);
            BinOp (isninf, Impl, numlit);
            BinOp
              ( BinOp (eq es (sl "Infinity"), Or, eq es (sl "+Infinity")),
                Impl,
                ispinf );
            BinOp (eq es (sl "-Infinity"), Impl, isninf);
            BinOp
              ( digits,
                Impl,
                BinOp (numlit, And, BinOp (not_ ispinf, And, not_ isninf)) );
          ]
        in
        let branches : (Expr.t list * Expr.t) list =
          [
            ([ not_ numlit ], Lit (Num Float.nan));
            ([ numlit; ispinf ], Lit (Num Float.infinity));
            ([ numlit; isninf ], Lit (Num Float.neg_infinity));
            ([ numlit; not_ ispinf; not_ isninf ], UnOp (ToNumberOp, es));
          ]
        in
        List.concat_map
          (fun (conds, r) ->
            match assume_all st (axioms @ conds) with
            | Some st' -> [ X.Return (st', v_of_expr r) ]
            | None -> [])
          branches

  (* ------------------------------------------------------------------ *)
  (* servpips_arith(op, a, b, site)  (E14, design section 4.4)           *)
  (* ------------------------------------------------------------------ *)

  let arith (st : st) (args : vt list) : outcome list =
    let op, a, b, site =
      match args with
      | [ op; a; b; site ] -> (string_arg "op" op, a, b, string_arg "site" site)
      | [ op; a; b ] -> (string_arg "op" op, a, b, "(none)")
      | _ -> fail_err "expected (op, a, b, site)"
    in
    let bop : BinOp.t =
      match op with
      | "+" -> FPlus
      | "-" -> FMinus
      | "*" -> FTimes
      | "/" -> FDiv
      | "%" -> FMod
      | _ -> fail_err "invalid operator %s" op
    in
    let ea = e_of a and eb = e_of b in
    let exact = Expr.BinOp (ea, bop, eb) in
    let ret st e = X.Return (st, guard (fun () -> S.eval_expr st e)) in
    let num f = Expr.Lit (Num f) in
    let nan = num Float.nan and inf = num Float.infinity
    and ninf = num Float.neg_infinity in
    let lit_num e =
      match e with
      | Expr.Lit (Num f) -> Some f
      | _ -> None
    in
    let nonfinite f = Float.is_nan f || Float.abs f = Float.infinity in
    let branch st conds r =
      match assume_all st conds with
      | Some st -> [ ret st r ]
      | None -> []
    in
    let zero = num 0. in
    let pos e = Expr.BinOp (zero, FLessThan, e)
    and neg e = Expr.BinOp (e, FLessThan, zero)
    and is0 e = Expr.BinOp (e, Equal, zero) in
    let la = lit_num ea and lb = lit_num eb in
    (* rule 1: both concrete -> IEEE (engine's concrete semantics) *)
    match (la, lb) with
    | Some _, Some _ -> [ ret st exact ]
    | _ when not E.symbolic -> [ ret st exact ]
    | _
      when Option.fold ~none:false ~some:nonfinite la
           || Option.fold ~none:false ~some:nonfinite lb -> (
        (* one operand is a non-finite literal, the other a (finite)
           symbolic number (A6) *)
        let is_nan = function Some f -> Float.is_nan f | None -> false in
        if is_nan la || is_nan lb then [ ret st nan ]
        else
          let sgn f = if f > 0. then inf else ninf in
          let flip f = if f > 0. then ninf else inf in
          match (op, la, lb) with
          | "+", Some f, _ | "+", _, Some f -> [ ret st (num f) ]
          | "-", Some f, _ -> [ ret st (num f) ]
          | "-", _, Some f -> [ ret st (num (-.f)) ]
          | "*", Some f, None | "*", None, Some f ->
              let x = if la = None then ea else eb in
              branch st [ pos x ] (sgn f)
              @ branch st [ neg x ] (flip f)
              @ branch st [ is0 x ] nan
          | "/", Some f, None ->
              branch st [ pos eb ] (sgn f)
              @ branch st [ neg eb ] (flip f)
              @ branch st [ is0 eb ] inf
              @ branch st [ is0 eb ] ninf
          | "/", None, Some _ -> [ ret st zero ]
          | "%", Some _, None -> [ ret st nan ]
          | "%", None, Some _ -> [ ret st ea ]
          | _ -> fail_err "impossible non-finite case")
    | _ ->
        (* rule 3: division / modulo by a possibly zero divisor *)
        let divides = op = "/" || op = "%" in
        let zero_f = is0 eb in
        let st_nonzero, zero_branches =
          if not divides then (Some st, [])
          else
            match lb with
            | Some f when f <> 0. -> (Some st, [])
            | _ ->
                let can_zero = sat st [ zero_f ] in
                let can_nonzero =
                  match lb with
                  | Some _ -> false (* literal 0 *)
                  | None -> sat st [ Expr.UnOp (Not, zero_f) ]
                in
                let zb =
                  if not can_zero then []
                  else if op = "%" then branch st [ zero_f ] nan
                  else
                    branch st [ zero_f; is0 ea ] nan
                    @ branch st [ zero_f; Expr.UnOp (Not, is0 ea) ] inf
                    @ branch st [ zero_f; Expr.UnOp (Not, is0 ea) ] ninf
                in
                let nz =
                  if not can_nonzero then None
                  else if not can_zero then Some st
                  else assume_all st [ Expr.UnOp (Not, zero_f) ]
                in
                (nz, zb)
        in
        let nonzero_branches =
          match st_nonzero with
          | None -> []
          | Some st -> (
              let int_in bound e =
                [
                  Expr.UnOp (IsInt, e);
                  BinOp (num (-.bound), FLessThan, e);
                  BinOp (e, FLessThan, num bound);
                ]
              in
              let exact_ok =
                match op with
                | "+" | "-" -> entails st (int_in two52 ea @ int_in two52 eb)
                | "*" -> entails st (int_in two26 ea @ int_in two26 eb)
                | "%" -> entails st [ UnOp (IsInt, ea); UnOp (IsInt, eb) ]
                | "/" -> (
                    match lb with
                    | Some f -> is_pow2_divisor f
                    | None -> false)
                | _ -> false
              in
              if exact_ok then [ ret st exact ]
              else
                (* rule 4: havoc, rule 5: overflow *)
                let label = site_label site in
                let name = Printf.sprintf "arith(%s)@%s#" op label in
                let st, k = next_count st name in
                let name = name ^ string_of_int k in
                let st, r, x = fresh_var st NumberType in
                emit_decl ~lvar:x ~name ~sort:"Num" ~kind:"havoc"
                  ~site:(`String site) ~k:(`Int k) ();
                Servpips.note ~code:"havoc" ~msg:name ~site ();
                let overflow =
                  if op = "%" then []
                  else
                    let maxv = num Float.max_float in
                    let p = Expr.BinOp (maxv, FLessThanEqual, exact) in
                    let n = Expr.BinOp (exact, FLessThanEqual, num (-.Float.max_float)) in
                    let pb = if sat st [ p ] then [ X.Return (S.copy st, vnum Float.infinity) ] else [] in
                    let nb = if sat st [ n ] then [ X.Return (S.copy st, vnum Float.neg_infinity) ] else [] in
                    if pb <> [] || nb <> [] then
                      Servpips.note ~code:"overflow-fork" ~msg:name ~site ();
                    pb @ nb
                in
                X.Return (st, r) :: overflow)
        in
        zero_branches @ nonzero_branches
end

(* ------------------------------------------------------------------------ *)
(* Registration                                                              *)
(* ------------------------------------------------------------------------ *)

let site_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.site st args);
  }

let emit_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.emit st args);
  }

let fresh_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.fresh st args);
  }

let assume_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.assume st args);
  }

let fn_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.fn st args);
  }

let define_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.define st args);
  }

let absent_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.absent st args);
  }

let mark_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.mark st args);
  }

let is_concrete_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.is_concrete st args);
  }

let arith_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.arith st args);
  }

let tonumber_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.tonumber st args);
  }

let rejected_h : X.handler =
  {
    X.run =
      (fun (type st vt)
           (module E : X.ENV with type st = st and type vt = vt)
           (st : st)
           (args : vt list) ->
        let module M = Make (E) in
        M.rejected st args);
  }

let initialised = ref false

let init () =
  if not !initialised then (
    initialised := true;
    List.iter
      (fun (n, h) -> X.register n h)
      [
        ("servpips_site", site_h);
        ("servpips_emit", emit_h);
        ("servpips_fresh", fresh_h);
        ("servpips_assume", assume_h);
        ("servpips_fn", fn_h);
        ("servpips_define", define_h);
        ("servpips_absent", absent_h);
        ("servpips_mark", mark_h);
        ("servpips_is_concrete", is_concrete_h);
        ("servpips_arith", arith_h);
        ("servpips_tonumber", tonumber_h);
        ("servpips_rejected", rejected_h);
      ])

let () = init ()

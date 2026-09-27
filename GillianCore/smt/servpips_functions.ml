(* SERVPIPS builtin functions (design 4.5, interface I6). See the .mli. *)

open Gil_syntax
module Sexp = Sexplib.Sexp

type smt = [ `Native of string | `Uf | `Defined ]
type spec = { args : Type.t option list; ret : Type.t; smt : smt }

let s_ = Type.StringType
let n_ = Type.NumberType
let b_ = Type.BooleanType

let fixed : (string * spec) list =
  let some = List.map Option.some in
  let nat name args ret = (name, { args = some args; ret; smt = `Native name }) in
  let uf name args ret = (name, { args = some args; ret; smt = `Uf }) in
  (* decision D-R2-2: conversions of values of any (JS) type, defined over
     the type constructors of GIL values *)
  let def name arity ret =
    (name, { args = List.init arity (fun _ -> None); ret; smt = `Defined })
  in
  [
    nat "str.replace_all" [ s_; s_; s_ ] s_;
    nat "str.contains" [ s_; s_ ] b_;
    nat "str.prefixof" [ s_; s_ ] b_;
    nat "str.suffixof" [ s_; s_ ] b_;
    nat "str.indexof" [ s_; s_; n_ ] n_;
    nat "str.substr" [ s_; n_; n_ ] s_;
    nat "str.from_int" [ n_ ] s_;
    nat "str.to_int" [ s_ ] n_;
    nat "str.in_re.numlit" [ s_ ] b_;
    uf "js.tonumber.isnan" [ s_ ] b_;
    uf "js.tonumber.ispinf" [ s_ ] b_;
    uf "js.tonumber.isninf" [ s_ ] b_;
    uf "js.tonumber.num" [ s_ ] n_;
    uf "decodeURIComponent" [ s_ ] s_;
    uf "decodeURIComponent.ok" [ s_ ] b_;
    uf "String.prototype.toLowerCase" [ s_ ] s_;
    uf "String.prototype.toUpperCase" [ s_ ] s_;
    uf "path.basename" [ s_ ] s_;
    uf "path.dirname" [ s_ ] s_;
    uf "path.extname" [ s_ ] s_;
    uf "path.normalize" [ s_ ] s_;
    uf "Date.prototype.toISOString" [ n_ ] s_;
    uf "JSON.quote" [ s_ ] s_;
    uf "js.num2str" [ n_ ] s_;
    uf "js.toUint32" [ n_ ] n_;
    def "js.tostring" 1 s_;
    def "js.toboolean" 1 b_;
    def "js.looseeq" 2 b_;
    (* round 3: Array.isArray of any value: false for a non-object,
       uninterpreted for an object (the engine decides it from the class of
       the location when it can) *)
    def "js.isarray" 1 b_;
    (* value-level conditional (the __servpips_fn "ite" of non-Boolean
       branches), monomorphic: SMT (ite c a b) *)
    ("ite.str", { args = some [ b_; s_; s_ ]; ret = s_; smt = `Native "ite" });
    ("ite.num", { args = some [ b_; n_; n_ ]; ret = n_; smt = `Native "ite" });
  ]

let table : (string, spec) Hashtbl.t =
  let t = Hashtbl.create 64 in
  List.iter (fun (n, s) -> Hashtbl.replace t n s) fixed;
  t

let path_join_prefix = "path.join/"

(* path.join/<n>: n string arguments *)
let path_join_arity name =
  let lp = String.length path_join_prefix in
  if String.length name > lp && String.sub name 0 lp = path_join_prefix then
    let digits = String.sub name lp (String.length name - lp) in
    if
      String.length digits <= 4
      && String.for_all (fun c -> c >= '0' && c <= '9') digits
      && (digits = "0" || digits.[0] <> '0')
    then Some (int_of_string digits)
    else None
  else None

let lookup name =
  match Hashtbl.find_opt table name with
  | Some s -> Some s
  | None -> (
      match path_join_arity name with
      | Some n ->
          Some { args = List.init n (fun _ -> Some s_); ret = s_; smt = `Uf }
      | None -> None)

let is_builtin name = Option.is_some (lookup name)

let is_bool name =
  match lookup name with
  | Some { ret = Type.BooleanType; _ } -> true
  | _ -> false

let all_names () =
  List.map fst fixed @ List.init 17 (fun n -> path_join_prefix ^ string_of_int n)

let func_of name (spec : spec) : Func.t =
  let params = List.mapi (fun i t -> ("x" ^ string_of_int i, t)) spec.args in
  {
    func_name = name;
    func_source_path = None;
    func_loc = None;
    func_num_params = List.length params;
    func_params = params;
    func_definition = Expr.Lit Literal.Nono;
  }

let funcs_for_prog () =
  List.filter_map
    (fun n -> Option.map (fun s -> (n, func_of n s)) (lookup n))
    (all_names ())

(* ------------------------------------------------------------------ *)
(* R_numlit: ES2023 StringNumericLiteral over UTF-16 code units        *)
(* ------------------------------------------------------------------ *)

(* StrWhiteSpaceChar: WhiteSpace (TAB, VT, FF, ZWNBSP, Zs) and LineTerminator
   (LF, CR, LS, PS). Zs (Unicode 15, Node 18/20): U+0020, U+00A0, U+1680,
   U+2000-U+200A, U+202F, U+205F, U+3000. *)
let ws_ranges =
  [
    (9, 13) (* TAB LF VT FF CR *);
    (32, 32);
    (160, 160);
    (5760, 5760);
    (8192, 8202);
    (8232, 8233) (* LS PS *);
    (8239, 8239);
    (8287, 8287);
    (12288, 12288);
    (65279, 65279) (* ZWNBSP *);
  ]

let is_ws_code c = List.exists (fun (lo, hi) -> lo <= c && c <= hi) ws_ranges

let numlit_regex_v =
  lazy
    (let a s = Sexp.Atom s in
     let app f args = Sexp.List (a f :: args) in
     let fc n = app "str.from_code" [ a (string_of_int n) ] in
     let ch n = app "str.to_re" [ fc n ] in
     let range lo hi = if lo = hi then ch lo else app "re.range" [ fc lo; fc hi ] in
     let union = function
       | [ x ] -> x
       | xs -> app "re.union" xs
     in
     let cat = function
       | [ x ] -> x
       | xs -> app "re.++" xs
     in
     let star x = app "re.*" [ x ] in
     let plus x = app "re.+" [ x ] in
     let opt x = app "re.opt" [ x ] in
     let lit s = cat (List.init (String.length s) (fun i -> ch (Char.code s.[i]))) in
     let c s = Char.code s.[0] in
     let one_of s = union (List.init (String.length s) (fun i -> ch (Char.code s.[i]))) in
     let ws = union (List.map (fun (lo, hi) -> range lo hi) ws_ranges) in
     let digit = range (c "0") (c "9") in
     let digits = plus digit in
     let sign = one_of "+-" in
     let exp = cat [ one_of "eE"; opt sign; digits ] in
     let unsigned =
       union
         [
           lit "Infinity";
           cat [ digits; ch (c "."); star digit; opt exp ];
           cat [ ch (c "."); digits; opt exp ];
           cat [ digits; opt exp ];
         ]
     in
     let decimal = cat [ opt sign; unsigned ] in
     let hex = union [ range (c "0") (c "9"); range (c "A") (c "F"); range (c "a") (c "f") ] in
     let nondecimal =
       union
         [
           cat [ ch (c "0"); one_of "bB"; plus (range (c "0") (c "1")) ];
           cat [ ch (c "0"); one_of "oO"; plus (range (c "0") (c "7")) ];
           cat [ ch (c "0"); one_of "xX"; plus hex ];
         ]
     in
     let numeric = union [ decimal; nondecimal ] in
     cat [ star ws; opt (cat [ numeric; star ws ]) ])

let numlit_regex () = Lazy.force numlit_regex_v
let numlit_regex_text () = Sexp.to_string (numlit_regex ())
let hello_json () = `Assoc [ ("str.in_re.numlit", `String (numlit_regex_text ())) ]

(* The same language, matched on a GIL byte string (one character per
   byte). *)
let numlit_matches (s : string) : bool =
  let n = String.length s in
  let code i = Char.code s.[i] in
  let lo = ref 0 in
  while !lo < n && is_ws_code (code !lo) do
    incr lo
  done;
  let hi = ref n in
  while !hi > !lo && is_ws_code (code (!hi - 1)) do
    decr hi
  done;
  let m = String.sub s !lo (!hi - !lo) in
  let len = String.length m in
  let is_digit ch = ch >= '0' && ch <= '9' in
  let all_from i p =
    i < len
    &&
    let ok = ref true in
    for j = i to len - 1 do
      if not (p m.[j]) then ok := false
    done;
    !ok
  in
  if len = 0 then true
  else if
    len >= 3
    && m.[0] = '0'
    && (match m.[1] with
       | 'b' | 'B' -> all_from 2 (fun ch -> ch = '0' || ch = '1')
       | 'o' | 'O' -> all_from 2 (fun ch -> ch >= '0' && ch <= '7')
       | 'x' | 'X' ->
           all_from 2 (fun ch ->
               is_digit ch || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
       | _ -> false)
  then true
  else
    let i = ref 0 in
    if !i < len && (m.[!i] = '+' || m.[!i] = '-') then incr i;
    let rest = String.sub m !i (len - !i) in
    if rest = "Infinity" then true
    else
      let digits_at j =
        let k = ref j in
        while !k < len && is_digit m.[!k] do
          incr k
        done;
        !k
      in
      let j1 = digits_at !i in
      let int_digits = j1 - !i in
      let j2, frac_digits =
        if j1 < len && m.[j1] = '.' then
          let j = digits_at (j1 + 1) in
          (j, j - (j1 + 1))
        else (j1, -1)
      in
      let mantissa_ok = int_digits > 0 || frac_digits > 0 in
      if not mantissa_ok then false
      else if j2 = len then true
      else if m.[j2] = 'e' || m.[j2] = 'E' then
        let k = ref (j2 + 1) in
        if !k < len && (m.[!k] = '+' || m.[!k] = '-') then incr k;
        let e_end = digits_at !k in
        e_end > !k && e_end = len
      else false

(* ------------------------------------------------------------------ *)
(* Concrete evaluation (SMT-LIB semantics on byte strings)             *)
(* ------------------------------------------------------------------ *)

let two53 = Z.shift_left Z.one 53

(* to_int of a finite real: floor *)
let floor_z (f : float) : Z.t option =
  if Float.is_finite f then Some (Z.of_float (Float.floor f)) else None

let num_of_z (z : Z.t) : Literal.t option =
  if Z.leq (Z.abs z) two53 then Some (Literal.Num (Z.to_float z)) else None

let replace_all s p r =
  if p = "" then s
  else
    let n = String.length s and m = String.length p in
    let b = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      if !i + m <= n && String.sub s !i m = p then (
        Buffer.add_string b r;
        i := !i + m)
      else (
        Buffer.add_char b s.[!i];
        incr i)
    done;
    Buffer.contents b

let find_from s p start =
  let n = String.length s and m = String.length p in
  let rec go i = if i + m > n then -1 else if String.sub s i m = p then i else go (i + 1) in
  go start

let contains s p = find_from s p 0 >= 0

let is_prefix p s =
  String.length p <= String.length s && String.sub s 0 (String.length p) = p

let is_suffix p s =
  let n = String.length s and m = String.length p in
  m <= n && String.sub s (n - m) m = p

(* Decision D-R2-2: the conversions of values of any JS type, on literals
   (ES2023 ToString, ToBoolean and IsLooselyEqual). [None] where the
   operation would call a JS method (ToPrimitive of an object) or the value
   is not a JS value. *)
let js_tostring (v : Literal.t) : string option =
  match v with
  | String s -> Some s
  | Num n -> Some (Utils.Arith_utils.js_number_to_string n)
  | Bool b -> Some (if b then "true" else "false")
  | Null -> Some "null"
  | Undefined -> Some "undefined"
  | _ -> None

let js_toboolean (v : Literal.t) : bool option =
  match v with
  | Bool b -> Some b
  | Num n -> Some (not (Float.is_nan n || n = 0.))
  | String s -> Some (s <> "")
  | Null | Undefined -> Some false
  | Loc _ -> Some true
  | _ -> None

let rec js_looseeq (a : Literal.t) (b : Literal.t) : bool option =
  let str_num s = Utils.Arith_utils.js_string_to_number s in
  match (a, b) with
  | (Undefined | Null), (Undefined | Null) -> Some true
  | (Undefined | Null), (Bool _ | Num _ | String _ | Loc _)
  | (Bool _ | Num _ | String _ | Loc _), (Undefined | Null) ->
      Some false
  | Bool x, Bool y -> Some (x = y)
  | Num x, Num y -> Some (x = y) (* IEEE: NaN <> NaN, 0 = -0 *)
  | String x, String y -> Some (String.equal x y)
  | Loc x, Loc y -> Some (String.equal x y)
  | Num x, String s -> Some (x = str_num s)
  | String s, Num y -> Some (str_num s = y)
  | Bool x, (Num _ | String _) -> js_looseeq (Num (if x then 1. else 0.)) b
  | (Num _ | String _), Bool y -> js_looseeq a (Num (if y then 1. else 0.))
  | _ -> None

let eval_concrete name (args : Literal.t list) : Literal.t option =
  let open Literal in
  match (name, args) with
  | "str.replace_all", [ String s; String p; String r ] ->
      Some (String (replace_all s p r))
  | "str.contains", [ String s; String p ] -> Some (Bool (contains s p))
  | "str.prefixof", [ String p; String s ] -> Some (Bool (is_prefix p s))
  | "str.suffixof", [ String p; String s ] -> Some (Bool (is_suffix p s))
  | "str.indexof", [ String s; String p; Num i ] -> (
      match floor_z i with
      | None -> None
      | Some zi ->
          let len = String.length s in
          if Z.lt zi Z.zero || Z.gt zi (Z.of_int len) then Some (Num (-1.))
          else
            let i = Z.to_int zi in
            Some (Num (float_of_int (find_from s p i))))
  | "str.substr", [ String s; Num i; Num n ] -> (
      match (floor_z i, floor_z n) with
      | Some zi, Some zn ->
          let len = String.length s in
          if Z.lt zi Z.zero || Z.geq zi (Z.of_int len) || Z.leq zn Z.zero then
            Some (String "")
          else
            let i = Z.to_int zi in
            let avail = len - i in
            let k = if Z.geq zn (Z.of_int avail) then avail else Z.to_int zn in
            Some (String (String.sub s i k))
      | _ -> None)
  | "str.from_int", [ Num i ] -> (
      match floor_z i with
      | None -> None
      | Some zi -> Some (String (if Z.lt zi Z.zero then "" else Z.to_string zi)))
  | "str.to_int", [ String s ] ->
      if s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s then
        num_of_z (Z.of_string s)
      else Some (Num (-1.))
  | "str.in_re.numlit", [ String s ] -> Some (Bool (numlit_matches s))
  | "ite.str", [ Bool c; (String _ as a); (String _ as b) ]
  | "ite.num", [ Bool c; (Num _ as a); (Num _ as b) ] ->
      Some (if c then a else b)
  | "js.tostring", [ v ] -> Option.map (fun s -> String s) (js_tostring v)
  | "js.toboolean", [ v ] -> Option.map (fun b -> Bool b) (js_toboolean v)
  | "js.looseeq", [ a; b ] -> Option.map (fun b -> Bool b) (js_looseeq a b)
  | "js.isarray", [ (String _ | Num _ | Bool _ | Null | Undefined) ] ->
      Some (Bool false)
  | _ -> None

(* Make the builtins known to the rest of GIL (typing of boolean expressions,
   the function environment). *)
let () =
  Expr.bool_func_hook := is_bool;
  Prog_env.Function_env.builtins := funcs_for_prog ()

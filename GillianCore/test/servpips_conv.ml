(* SERVPIPS decision D-R2-2 unit tests (WP3): the defined builtins
   js.tostring, js.toboolean and js.looseeq (conversions of values whose JS
   type is a union, without forking per type). Their concrete evaluation
   and their SMT definitions are checked against Node
   (servpips_conv_conformance.json, gen_conv_conformance.js); their typing
   and reductions are checked directly. *)

open Gil_syntax
module SF = Smt.Servpips_functions
module Reduction = Engine.Reduction
module Typing = Engine.Typing
module Type_env = Engine.Type_env

let str s = Expr.Lit (String s)
let num f = Expr.Lit (Num f)
let lv x = Expr.LVar x
let app n args = Expr.FuncApp (n, args)
let eq a b = Expr.BinOp (a, Equal, b)
let not_ e = Expr.UnOp (Not, e)

let with_env f =
  Prog_env.Datatype_env.using
    (Prog_env.Datatype_env.make' (Hashtbl.create 1))
    (fun () ->
      Prog_env.Function_env.using
        (Prog_env.Function_env.make' (Hashtbl.create 1))
        f)

(* ------------------------------------------------------------------ *)
(* Node values                                                         *)
(* ------------------------------------------------------------------ *)

let conformance =
  lazy
    (let candidates =
       (match Sys.getenv_opt "SERVPIPS_CONV_CONFORMANCE" with
       | Some f -> [ f ]
       | None -> [])
       @ [
           "servpips_conv_conformance.json";
           "GillianCore/test/servpips_conv_conformance.json";
         ]
     in
     match List.find_opt Sys.file_exists candidates with
     | Some f -> Yojson.Safe.from_file f
     | None -> Alcotest.fail "servpips_conv_conformance.json not found")

let field name =
  match Lazy.force conformance with
  | `Assoc l -> (
      match List.assoc_opt name l with
      | Some (`List xs) -> xs
      | _ -> Alcotest.fail ("conformance field missing: " ^ name))
  | _ -> Alcotest.fail "bad conformance file"

let of_bits hex = Int64.float_of_bits (Int64.of_string ("0x" ^ hex))

let values : Literal.t array Lazy.t =
  lazy
    (Array.of_list
       (List.map
          (function
            | `Assoc l -> (
                match (List.assoc_opt "t" l, List.assoc_opt "v" l) with
                | Some (`String "str"), Some (`String s) -> Literal.String s
                | Some (`String "num"), Some (`String h) -> Literal.Num (of_bits h)
                | Some (`String "bool"), Some (`Bool b) -> Literal.Bool b
                | Some (`String "null"), _ -> Literal.Null
                | Some (`String "undef"), _ -> Literal.Undefined
                | _ -> Alcotest.fail "bad value")
            | _ -> Alcotest.fail "bad value")
          (field "values")))

let strings name =
  Array.of_list
    (List.map
       (function
         | `String s -> s
         | _ -> Alcotest.fail ("bad " ^ name))
       (field name))

let bools_of = function
  | `Bool b -> b
  | _ -> Alcotest.fail "bad boolean"

let booleans name = Array.of_list (List.map bools_of (field name))

let matrix name =
  Array.of_list
    (List.map
       (function
         | `List row -> Array.of_list (List.map bools_of row)
         | _ -> Alcotest.fail ("bad " ^ name))
       (field name))

let pp_lit = Fmt.to_to_string Literal.pp

(* ------------------------------------------------------------------ *)
(* Table                                                               *)
(* ------------------------------------------------------------------ *)

let test_table () =
  let chk n arity ret =
    match SF.lookup n with
    | Some { args; ret = r; smt = `Defined } ->
        Alcotest.(check int) (n ^ " arity") arity (List.length args);
        Alcotest.(check bool) (n ^ " any-typed") true (List.for_all Option.is_none args);
        Alcotest.(check bool) (n ^ " result") true (Type.equal r ret)
    | _ -> Alcotest.fail (n ^ " is not a defined builtin")
  in
  chk "js.tostring" 1 Type.StringType;
  chk "js.toboolean" 1 Type.BooleanType;
  chk "js.looseeq" 2 Type.BooleanType;
  Alcotest.(check bool) "is_bool js.toboolean" true (SF.is_bool "js.toboolean");
  Alcotest.(check bool) "is_bool js.looseeq" true (SF.is_bool "js.looseeq");
  Alcotest.(check bool) "is_bool js.tostring" false (SF.is_bool "js.tostring");
  Alcotest.(check bool) "boolean expression" true
    (Expr.is_boolean_expr (app "js.toboolean" [ lv "#v" ]));
  Alcotest.(check bool) "in Function_env.builtins" true
    (List.mem_assoc "js.looseeq" !Prog_env.Function_env.builtins)

(* ------------------------------------------------------------------ *)
(* Concrete evaluation vs Node (incl. NaN, +/-Infinity, -0)            *)
(* ------------------------------------------------------------------ *)

let test_eval_vs_node () =
  let vs = Lazy.force values in
  let ts = strings "tostring" and bs = booleans "toboolean" in
  let le = matrix "looseeq" in
  let bad = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun m ->
        incr bad;
        if !bad <= 20 then Fmt.epr "%s@." m)
      fmt
  in
  Array.iteri
    (fun i v ->
      (match SF.eval_concrete "js.tostring" [ v ] with
      | Some (String s) when s = ts.(i) -> ()
      | r ->
          fail "js.tostring(%s) = %s, Node %S" (pp_lit v)
            (Option.fold ~none:"-" ~some:pp_lit r)
            ts.(i));
      (match SF.eval_concrete "js.toboolean" [ v ] with
      | Some (Bool b) when b = bs.(i) -> ()
      | _ -> fail "js.toboolean(%s) <> Node %b" (pp_lit v) bs.(i));
      Array.iteri
        (fun j w ->
          match SF.eval_concrete "js.looseeq" [ v; w ] with
          | Some (Bool b) when b = le.(i).(j) -> ()
          | _ -> fail "js.looseeq(%s, %s) <> Node %b" (pp_lit v) (pp_lit w) le.(i).(j))
        vs)
    vs;
  (* objects: ToBoolean true, ToString / == with a primitive unspecified *)
  let l = Literal.Loc "$l_test" in
  Alcotest.(check bool) "toboolean(object)" true
    (SF.eval_concrete "js.toboolean" [ l ] = Some (Bool true));
  Alcotest.(check bool) "tostring(object) unspecified" true
    (SF.eval_concrete "js.tostring" [ l ] = None);
  Alcotest.(check bool) "object == 1 unspecified" true
    (SF.eval_concrete "js.looseeq" [ l; Num 1. ] = None);
  Alcotest.(check bool) "object == null" true
    (SF.eval_concrete "js.looseeq" [ l; Null ] = Some (Bool false));
  Alcotest.(check bool) "object == itself" true
    (SF.eval_concrete "js.looseeq" [ l; l ] = Some (Bool true));
  Alcotest.(check int) "mismatches with Node" 0 !bad

(* ------------------------------------------------------------------ *)
(* SMT definitions vs Node                                             *)
(* ------------------------------------------------------------------ *)

let sat fs =
  Smt.servpips_enable ();
  match Smt.check_sat (Expr.Set.of_list fs) (Hashtbl.create 1) with
  | Some m -> if Smt.is_unknown_model m then `Unknown else `Sat
  | None -> `Unsat

let finite = function
  | Literal.Num f -> Float.is_finite f
  | _ -> true

let is_digits s =
  let n = String.length s in
  n >= 1 && n <= 15 && String.for_all (fun c -> c >= '0' && c <= '9') s

let is_ws s = String.for_all (fun c -> c = ' ' || (c >= '\t' && c <= '\r')) s

(* the SMT definitions use uninterpreted functions for Number::toString of
   non-integers / magnitudes >= 1e21 and for ToNumber of strings other than
   digit strings and white space: there, only consistency with Node is
   required (the true value is allowed), elsewhere exactness *)
let exact_tostring = function
  | Literal.Num f -> Float.is_integer f && Float.abs f < 1e21
  | _ -> true

let exact_looseeq a b =
  let num_like = function
    | Literal.Num _ | Bool _ -> true
    | _ -> false
  in
  match ((a : Literal.t), (b : Literal.t)) with
  | String s, o when num_like o -> is_digits s || is_ws s
  | o, String s when num_like o -> is_digits s || is_ws s
  | _ -> true

let test_smt_vs_node () =
  with_env @@ fun () ->
  let vs = Lazy.force values in
  let ts = strings "tostring" and bs = booleans "toboolean" in
  let le = matrix "looseeq" in
  let x = lv "#x" and y = lv "#y" in
  let bad = ref 0 and checked = ref 0 and exact = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun m ->
        incr bad;
        if !bad <= 20 then Fmt.epr "%s@." m)
      fmt
  in
  (* [f] (a formula over #x, #y) holds on the values: consistent (sat) and,
     when [ex], exact (the negation is unsat) *)
  let check name ~ex binds f =
    incr checked;
    (match sat (binds @ [ f ]) with
    | `Sat -> ()
    | _ -> fail "%s: the Node value is excluded" name);
    if ex then (
      incr exact;
      match sat (binds @ [ not_ f ]) with
      | `Unsat -> ()
      | _ -> fail "%s: not exact" name)
  in
  Array.iteri
    (fun i v ->
      if finite v then (
        let bx = [ eq x (Lit v) ] in
        let nm = pp_lit v in
        check ("tostring " ^ nm) ~ex:(exact_tostring v) bx
          (eq (app "js.tostring" [ x ]) (str ts.(i)));
        check ("toboolean " ^ nm) ~ex:true bx
          (eq (app "js.toboolean" [ x ]) (Lit (Bool bs.(i))));
        Array.iteri
          (fun j w ->
            if finite w then
              check
                (Printf.sprintf "looseeq %s %s" nm (pp_lit w))
                ~ex:(exact_looseeq v w)
                (bx @ [ eq y (Lit w) ])
                (eq (app "js.looseeq" [ x; y ]) (Lit (Bool le.(i).(j)))))
          vs))
    vs;
  (* objects (abstract locations) *)
  let o = Expr.ALoc "#o1" and o2 = Expr.ALoc "#o2" in
  check "toboolean(object)" ~ex:true [ eq x o ] (app "js.toboolean" [ x ]);
  check "object == null false" ~ex:true [ eq x o ]
    (not_ (app "js.looseeq" [ x; Lit Null ]));
  check "object == same object" ~ex:true [ eq x o; eq y o ]
    (app "js.looseeq" [ x; y ]);
  check "objects: == is identity" ~ex:true
    [ eq x o; eq y o2; not_ (eq o o2) ]
    (not_ (app "js.looseeq" [ x; y ]));
  (* a union: "order " ++ js.tostring(x) = "order 12" has exactly the
     solutions "12" and 12 among the primitives *)
  let u = [ eq (Expr.BinOp (str "order ", StrCat, app "js.tostring" [ x ])) (str "order 12") ] in
  Alcotest.(check bool) "union: string solution" true (sat (u @ [ eq x (str "12") ]) = `Sat);
  Alcotest.(check bool) "union: number solution" true (sat (u @ [ eq x (num 12.) ]) = `Sat);
  Alcotest.(check bool) "union: no boolean solution" true
    (sat (u @ [ Expr.BinOp (UnOp (TypeOf, x), Equal, Lit (Type BooleanType)) ]) = `Unsat);
  Alcotest.(check bool) "union: no null / undefined solution" true
    (sat (u @ [ Expr.BinOp (eq x (Lit Null), Or, eq x (Lit Undefined)) ]) = `Unsat);
  Fmt.epr "SMT conformance: %d checks, %d exact@." !checked !exact;
  Alcotest.(check int) "SMT mismatches with Node" 0 !bad

(* ------------------------------------------------------------------ *)
(* Reduction and typing                                                *)
(* ------------------------------------------------------------------ *)

let test_reduction_typing () =
  with_env @@ fun () ->
  let gamma = Type_env.init () in
  Type_env.update gamma "#s" Type.StringType;
  Type_env.update gamma "#b" Type.BooleanType;
  Type_env.update gamma "#u" Type.UndefinedType;
  Type_env.update gamma "#o" Type.ObjectType;
  let red e = Reduction.reduce_lexpr ~gamma e in
  let same name a b = Alcotest.(check bool) name true (Expr.equal (red a) b) in
  same "tostring literal" (app "js.tostring" [ num 12. ]) (str "12");
  same "tostring NaN" (app "js.tostring" [ num Float.nan ]) (str "NaN");
  same "tostring of a string" (app "js.tostring" [ lv "#s" ]) (lv "#s");
  same "tostring of undefined" (app "js.tostring" [ lv "#u" ]) (str "undefined");
  same "tostring of a union kept" (app "js.tostring" [ lv "#v" ]) (app "js.tostring" [ lv "#v" ]);
  same "toboolean of a boolean" (app "js.toboolean" [ lv "#b" ]) (lv "#b");
  same "toboolean of an object" (app "js.toboolean" [ lv "#o" ]) Expr.true_;
  same "toboolean of a string" (app "js.toboolean" [ lv "#s" ]) (not_ (eq (lv "#s") (str "")));
  same "looseeq literals" (app "js.looseeq" [ num 5.; str "5" ]) Expr.true_;
  same "looseeq NaN" (app "js.looseeq" [ lv "#v"; num Float.nan ]) Expr.false_;
  let ty e = fst (Typing.type_lexpr gamma e) in
  let t = Alcotest.testable (Fmt.option Type.pp) (Option.equal Type.equal) in
  Alcotest.check t "js.tostring(any) : Str" (Some Type.StringType) (ty (app "js.tostring" [ lv "#v" ]));
  Alcotest.check t "js.looseeq(any, any) : Bool" (Some Type.BooleanType)
    (ty (app "js.looseeq" [ lv "#v"; lv "#o" ]));
  (* reverse typing does not constrain an argument of any type *)
  let g2 = Type_env.init () in
  match
    Typing.reverse_type_lexpr true g2
      [ (eq (app "js.tostring" [ lv "#a" ]) (str "x"), Type.BooleanType) ]
  with
  | Some g' ->
      Alcotest.(check bool) "no type inferred for the argument" true (Type_env.get g' "#a" = None)
  | None -> Alcotest.fail "reverse typing failed"

(* ------------------------------------------------------------------ *)
(* ite.str / ite.num and string lengths of builtin applications        *)
(* ------------------------------------------------------------------ *)

let test_ite_and_lengths () =
  with_env @@ fun () ->
  let gamma = Type_env.init () in
  Type_env.update gamma "#c" Type.BooleanType;
  Type_env.update gamma "#s" Type.StringType;
  Type_env.update gamma "#x" Type.NumberType;
  let red e = Reduction.reduce_lexpr ~gamma e in
  let same name a b = Alcotest.(check bool) name true (Expr.equal (red a) b) in
  let ite_s c a b = app "ite.str" [ c; a; b ] in
  same "ite.str literal condition" (ite_s (Lit (Bool true)) (lv "#s") (str "a")) (lv "#s");
  same "ite.num equal branches" (app "ite.num" [ lv "#c"; num 1.; num 1. ]) (num 1.);
  same "ite.str on literals" (ite_s (Lit (Bool false)) (str "t") (str "f")) (str "f");
  let t = Alcotest.testable (Fmt.option Type.pp) (Option.equal Type.equal) in
  Alcotest.check t "ite.str : Str" (Some Type.StringType)
    (fst (Typing.type_lexpr gamma (ite_s (lv "#c") (str "true") (str "false"))));
  Alcotest.check t "ite.num : Num" (Some Type.NumberType)
    (fst (Typing.type_lexpr gamma (app "ite.num" [ lv "#c"; num 1.; lv "#x" ])));
  let j = ite_s (lv "#c") (str "true") (str "false") in
  Alcotest.(check bool) "ite.str: only true/false" true
    (sat [ Expr.BinOp (UnOp (TypeOf, lv "#c"), Equal, Lit (Type BooleanType)); eq j (str "x") ] = `Unsat);
  Alcotest.(check bool) "ite.str: true when c" true
    (sat [ eq (lv "#c") (Lit (Bool true)); eq j (str "true") ] = `Sat);
  Alcotest.(check bool) "ite.str: not false when c" true
    (sat [ eq (lv "#c") (Lit (Bool true)); eq j (str "false") ] = `Unsat);
  (* string length / character of builtin applications: unknown in
     Reduction (no exception), str.len of the term in SMT *)
  Utils.Config.servpips_semantics := true;
  let u = app "decodeURIComponent" [ app "str.replace_all" [ lv "#s"; str "+"; str " " ] ] in
  let ln = Expr.UnOp (StrLen, u) in
  same "length of a UF string kept" ln ln;
  let n2s = Expr.UnOp (StrLen, UnOp (ToStringOp, lv "#x")) in
  same "length of num_to_string kept" n2s n2s;
  same "length of js.tostring kept" (Expr.UnOp (StrLen, app "js.tostring" [ lv "#v" ]))
    (Expr.UnOp (StrLen, app "js.tostring" [ lv "#v" ]));
  let nth = Expr.BinOp (UnOp (ToStringOp, lv "#x"), StrNth, num 0.) in
  Alcotest.(check bool) "character of num_to_string: no exception" true
    (match red nth with
    | _ -> true
    | exception _ -> false);
  Alcotest.(check bool) "SMT: length of num_to_string 12 is 2" true
    (sat [ eq (lv "#x") (num 12.); Expr.BinOp (UnOp (StrLen, UnOp (ToStringOp, lv "#x")), Equal, num 3.) ]
    = `Unsat)

(* ------------------------------------------------------------------ *)
(* Round 3: js.isarray, specialised js.tostring encodings                *)
(* ------------------------------------------------------------------ *)

let test_round3 () =
  with_env @@ fun () ->
  let x = lv "#x" in
  let ty e t = Expr.BinOp (UnOp (TypeOf, e), Equal, Lit (Type t)) in
  let or_ a b = Expr.BinOp (a, Or, b) in
  let chk name expected fs =
    let r = sat fs in
    Alcotest.(check bool) name true (r = expected)
  in
  (* js.isarray: in the table, Boolean, false on every non-object literal *)
  (match SF.lookup "js.isarray" with
  | Some { args = [ None ]; ret = Type.BooleanType; smt = `Defined } -> ()
  | _ -> Alcotest.fail "js.isarray is not a defined Any -> Bool builtin");
  Alcotest.(check bool) "is_bool js.isarray" true (SF.is_bool "js.isarray");
  List.iter
    (fun l ->
      Alcotest.(check bool) ("isarray " ^ pp_lit l) true
        (SF.eval_concrete "js.isarray" [ l ] = Some (Literal.Bool false)))
    Literal.[ String "a"; Num 1.; Num Float.nan; Bool true; Null; Undefined ];
  Alcotest.(check bool) "isarray of a location: not evaluated" true
    (SF.eval_concrete "js.isarray" [ Literal.Loc "$l_a" ] = None);
  let o = Expr.ALoc "#o1" in
  chk "SMT isarray(string) unsat" `Unsat [ eq x (str "a"); app "js.isarray" [ x ] ];
  chk "SMT isarray(object) sat" `Sat [ eq x o; app "js.isarray" [ x ] ];
  chk "SMT not isarray(object) sat" `Sat [ eq x o; not_ (app "js.isarray" [ x ]) ];
  chk "SMT isarray of a Str | Undefined value unsat" `Unsat
    [ or_ (ty x StringType) (eq x (Lit Undefined)); app "js.isarray" [ x ] ];
  let gamma = Type_env.init () in
  Type_env.update gamma "#s" Type.StringType;
  Alcotest.(check bool) "reduction: isarray of a Str is false" true
    (Expr.equal (Reduction.reduce_lexpr ~gamma (app "js.isarray" [ lv "#s" ])) Expr.false_);
  (* js.tostring of a variable the query restricts to non-number types
     (js.tostring.nonum): the same answers as the full definition *)
  let m = or_ (ty x StringType) (eq x (Lit Undefined)) in
  let ts = app "js.tostring" [ x ] in
  chk "mask: a string gives itself" `Sat [ m; eq ts (str "12") ];
  chk "mask: the string \"undefined\"" `Sat [ m; eq ts (str "undefined"); not_ (eq x (Lit Undefined)) ];
  chk "mask: undefined gives \"undefined\"" `Unsat [ m; eq x (Lit Undefined); not_ (eq ts (str "undefined")) ];
  chk "mask: exact on strings" `Unsat [ m; eq ts (str "abc"); eq x (str "abd") ];
  chk "mask: no number" `Unsat [ m; eq x (num 12.) ];
  (* a mask that allows numbers keeps the numeric branch *)
  chk "mask with Num: the number 7" `Sat
    [ or_ (ty x NumberType) (eq x (Lit Undefined)); eq ts (str "7"); ty x NumberType ];
  chk "mask with Num: exact on 7" `Unsat
    [ or_ (ty x NumberType) (eq x (Lit Undefined)); eq x (num 7.); not_ (eq ts (str "7")) ];
  (* a known native type: the conversion of that type *)
  let g = Hashtbl.create 2 in
  Hashtbl.replace g "#s" Type.StringType;
  Hashtbl.replace g "#n" Type.NumberType;
  let sat_g fs =
    match Smt.check_sat (Expr.Set.of_list fs) g with
    | Some mm -> if Smt.is_unknown_model mm then `Unknown else `Sat
    | None -> `Unsat
  in
  let s = lv "#s" in
  Alcotest.(check bool) "native Str: identity" true
    (sat_g [ eq s (str "q"); not_ (eq (app "js.tostring" [ s ]) (str "q")) ] = `Unsat);
  Alcotest.(check bool) "native Num: integer text" true
    (sat_g [ eq (lv "#n") (num 42.); not_ (eq (app "js.tostring" [ lv "#n" ]) (str "42")) ] = `Unsat)

let tests : unit Alcotest.test_case list =
  [
    ("round 3: js.isarray, specialised js.tostring encodings", `Quick, test_round3);
    ("defined builtins in the table", `Quick, test_table);
    ("concrete conversions vs Node", `Quick, test_eval_vs_node);
    ("SMT definitions vs Node", `Quick, test_smt_vs_node);
    ("reduction and typing", `Quick, test_reduction_typing);
    ("ite.str / ite.num, lengths of builtin strings", `Quick, test_ite_and_lengths);
  ]

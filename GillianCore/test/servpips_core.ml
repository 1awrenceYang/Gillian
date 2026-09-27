(* SERVPIPS engine-core unit tests (WP1): builtin function table (E5), SMT
   encodings (E5, E6), NaN semantics (E7), StringToNumber (E16),
   Number::toString (E8), PFS index (E19). *)

open Gil_syntax
module SF = Smt.Servpips_functions
module Reduction = Engine.Reduction
module Typing = Engine.Typing
module Type_env = Engine.Type_env
module Config = Utils.Config
module Arith_utils = Utils.Arith_utils

let str s = Expr.Lit (String s)
let num f = Expr.Lit (Num f)
let lv x = Expr.LVar x
let app n args = Expr.FuncApp (n, args)
let lit = Alcotest.testable Literal.pp Literal.equal

(* Run [f] inside empty datatype / function environments. *)
let with_env f =
  Prog_env.Datatype_env.using
    (Prog_env.Datatype_env.make' (Hashtbl.create 1))
    (fun () ->
      Prog_env.Function_env.using
        (Prog_env.Function_env.make' (Hashtbl.create 1))
        f)

(* ------------------------------------------------------------------ *)
(* Builtin table                                                       *)
(* ------------------------------------------------------------------ *)

let test_table () =
  let names =
    [
      "str.replace_all"; "str.contains"; "str.prefixof"; "str.suffixof";
      "str.indexof"; "str.substr"; "str.from_int"; "str.to_int";
      "str.in_re.numlit"; "js.tonumber.isnan"; "js.tonumber.ispinf";
      "js.tonumber.isninf"; "js.tonumber.num"; "decodeURIComponent";
      "decodeURIComponent.ok"; "String.prototype.toLowerCase";
      "String.prototype.toUpperCase"; "path.basename"; "path.dirname";
      "path.extname"; "path.normalize"; "path.join/0"; "path.join/3";
      "path.join/25"; "Date.prototype.toISOString"; "JSON.quote";
      "js.num2str"; "js.toUint32";
    ]
  in
  List.iter
    (fun n -> Alcotest.(check bool) ("builtin " ^ n) true (SF.is_builtin n))
    names;
  List.iter
    (fun n -> Alcotest.(check bool) ("not builtin " ^ n) false (SF.is_builtin n))
    [ "str.len"; "path.join/"; "path.join/01"; "path.join/x"; "foo" ];
  let arity n = List.length (Option.get (SF.lookup n)).args in
  Alcotest.(check int) "path.join/3 arity" 3 (arity "path.join/3");
  Alcotest.(check int) "str.indexof arity" 3 (arity "str.indexof");
  let funcs = SF.funcs_for_prog () in
  Alcotest.(check bool)
    "funcs_for_prog has str.contains" true
    (List.mem_assoc "str.contains" funcs);
  Alcotest.(check bool)
    "funcs_for_prog has path.join/16" true
    (List.mem_assoc "path.join/16" funcs);
  Alcotest.(check bool)
    "Function_env.builtins installed" true
    (List.mem_assoc "js.num2str" !Prog_env.Function_env.builtins)

let test_is_boolean () =
  let b e = Expr.is_boolean_expr e in
  Alcotest.(check bool) "str.contains is boolean" true
    (b (app "str.contains" [ lv "#s"; str "a" ]));
  Alcotest.(check bool) "numlit is boolean" true
    (b (app "str.in_re.numlit" [ lv "#s" ]));
  Alcotest.(check bool) "js.tonumber.isnan is boolean" true
    (b (app "js.tonumber.isnan" [ lv "#s" ]));
  Alcotest.(check bool) "not (prefixof) is boolean" true
    (b (UnOp (Not, app "str.prefixof" [ str "a"; lv "#s" ])));
  Alcotest.(check bool) "str.substr is not boolean" false
    (b (app "str.substr" [ lv "#s"; num 0.; num 1. ]));
  Alcotest.(check bool) "js.num2str is not boolean" false
    (b (app "js.num2str" [ lv "#x" ]))

let test_eval_concrete () =
  let ev n args = SF.eval_concrete n args in
  let s x = Literal.String x and n x = Literal.Num x and b x = Literal.Bool x in
  let chk name exp got = Alcotest.(check (option lit)) name exp got in
  chk "replace_all" (Some (s "xbxb")) (ev "str.replace_all" [ s "abab"; s "a"; s "x" ]);
  chk "replace_all empty pattern" (Some (s "ab")) (ev "str.replace_all" [ s "ab"; s ""; s "x" ]);
  chk "replace_all non-overlapping" (Some (s "ba")) (ev "str.replace_all" [ s "aaa"; s "aa"; s "b" ]);
  chk "contains" (Some (b true)) (ev "str.contains" [ s "hello"; s "ll" ]);
  chk "contains empty" (Some (b true)) (ev "str.contains" [ s "x"; s "" ]);
  chk "prefixof" (Some (b true)) (ev "str.prefixof" [ s "he"; s "hello" ]);
  chk "prefixof no" (Some (b false)) (ev "str.prefixof" [ s "lo"; s "hello" ]);
  chk "suffixof" (Some (b true)) (ev "str.suffixof" [ s "lo"; s "hello" ]);
  chk "indexof" (Some (n 2.)) (ev "str.indexof" [ s "hello"; s "l"; n 0. ]);
  chk "indexof from" (Some (n 3.)) (ev "str.indexof" [ s "hello"; s "l"; n 3. ]);
  chk "indexof floor" (Some (n 2.)) (ev "str.indexof" [ s "hello"; s "l"; n 2.5 ]);
  chk "indexof missing" (Some (n (-1.))) (ev "str.indexof" [ s "hello"; s "z"; n 0. ]);
  chk "indexof empty at len" (Some (n 5.)) (ev "str.indexof" [ s "hello"; s ""; n 5. ]);
  chk "indexof out of range" (Some (n (-1.))) (ev "str.indexof" [ s "hello"; s ""; n 6. ]);
  chk "indexof negative" (Some (n (-1.))) (ev "str.indexof" [ s "hello"; s "h"; n (-1.) ]);
  chk "indexof nan" None (ev "str.indexof" [ s "hello"; s "h"; n Float.nan ]);
  chk "substr" (Some (s "ell")) (ev "str.substr" [ s "hello"; n 1.; n 3. ]);
  chk "substr clipped" (Some (s "lo")) (ev "str.substr" [ s "hello"; n 3.; n 10. ]);
  chk "substr out" (Some (s "")) (ev "str.substr" [ s "hello"; n 5.; n 1. ]);
  chk "substr neg len" (Some (s "")) (ev "str.substr" [ s "hello"; n 1.; n (-1.) ]);
  chk "from_int" (Some (s "42")) (ev "str.from_int" [ n 42. ]);
  chk "from_int floor" (Some (s "42")) (ev "str.from_int" [ n 42.9 ]);
  chk "from_int negative" (Some (s "")) (ev "str.from_int" [ n (-1.) ]);
  chk "from_int big" (Some (s "1000000000000000000000")) (ev "str.from_int" [ n 1e21 ]);
  chk "to_int" (Some (n 7.)) (ev "str.to_int" [ s "007" ]);
  chk "to_int bad" (Some (n (-1.))) (ev "str.to_int" [ s "1a" ]);
  chk "to_int empty" (Some (n (-1.))) (ev "str.to_int" [ s "" ]);
  chk "to_int too big" None (ev "str.to_int" [ s "99999999999999999999" ]);
  chk "UF not evaluated" None (ev "js.num2str" [ n 1. ]);
  chk "ill-typed" None (ev "str.contains" [ n 1.; s "a" ])

(* ES2023 StringNumericLiteral: numlit(s) <=> not (isNaN (Number s)); the
   Node values were checked with Node 20 (see gen_conformance.js). *)
let numlit_samples =
  [
    ("", true); (" ", true); ("12", true); ("  12  ", true); ("\t\n12\r", true);
    ("1_000", false); ("inf", false); ("Infinity", true); ("-Infinity", true);
    ("+Infinity", true); ("  -Infinity ", true); ("infinity", false);
    ("0x1p3", false); ("0x1F", true); ("0X1f", true); ("-0x10", false);
    ("0b11", true); ("0B2", false); ("0o7", true); ("0o8", false);
    ("1e3", true); ("1E+3", true); ("1e-3", true); ("1e", false);
    (".5", true); ("5.", true); (".", false); ("+.5", true); ("-5.e2", true);
    ("--5", false); ("1 2", false); ("nan", false); ("NaN", false);
    ("0x", false); ("00012", true); ("\xa012", true); ("12a", false);
    ("\x0b12\x0c", true); ("1.2.3", false); ("e5", false);
  ]

let test_numlit_concrete () =
  List.iter
    (fun (s, exp) ->
      Alcotest.(check bool) (Printf.sprintf "numlit %S" s) exp (SF.numlit_matches s))
    numlit_samples

(* ------------------------------------------------------------------ *)
(* SMT                                                                  *)
(* ------------------------------------------------------------------ *)

let gamma_of l =
  let g = Hashtbl.create 8 in
  List.iter (fun (x, t) -> Hashtbl.replace g x t) l;
  g

let sat ?(gamma = []) fs =
  Smt.servpips_enable ();
  match Smt.check_sat (Expr.Set.of_list fs) (gamma_of gamma) with
  | Some m -> if Smt.is_unknown_model m then `Unknown else `Sat
  | None -> `Unsat

let res =
  Alcotest.testable
    (fun fmt r ->
      Fmt.string fmt
        (match r with
        | `Sat -> "sat"
        | `Unsat -> "unsat"
        | `Unknown -> "unknown"))
    ( = )

let eq a b = Expr.BinOp (a, Equal, b)
let slen e = Expr.UnOp (StrLen, e)
let gs = [ ("#s", Type.StringType) ]
let gx = [ ("#x", Type.NumberType) ]

let test_smt_builtins () =
  let chk name exp fs gamma = Alcotest.check res name exp (sat ~gamma fs) in
  chk "contains + length 1 unsat" `Unsat
    [ app "str.contains" [ lv "#s"; str "ab" ]; eq (slen (lv "#s")) (num 1.) ]
    gs;
  chk "contains sat" `Sat [ app "str.contains" [ lv "#s"; str "ab" ] ] gs;
  chk "prefixof / suffixof" `Unsat
    [
      app "str.prefixof" [ str "a"; lv "#s" ];
      app "str.suffixof" [ str "b"; lv "#s" ];
      eq (slen (lv "#s")) (num 1.);
    ]
    gs;
  chk "replace_all" `Sat
    [ eq (lv "#s") (str "abab"); eq (app "str.replace_all" [ lv "#s"; str "a"; str "x" ]) (str "xbxb") ]
    gs;
  chk "replace_all wrong" `Unsat
    [ eq (lv "#s") (str "abab"); eq (app "str.replace_all" [ lv "#s"; str "a"; str "x" ]) (str "xbab") ]
    gs;
  (* z3's string solver may answer unknown here; unknown is treated as sat *)
  Alcotest.(check bool) "replace_all hard: not unsat" true
    (sat ~gamma:gs
       [ eq (app "str.replace_all" [ lv "#s"; str "a"; str "" ]) (str "bc"); eq (slen (lv "#s")) (num 4.) ]
    <> `Unsat);
  chk "indexof" `Unsat
    [ eq (app "str.indexof" [ str "hello"; lv "#s"; num 0. ]) (num 1.); eq (lv "#s") (str "l") ]
    gs;
  chk "substr" `Sat
    [ eq (app "str.substr" [ lv "#s"; num 1.; num 2. ]) (str "el"); eq (slen (lv "#s")) (num 5.) ]
    gs;
  chk "from_int / to_int" `Unsat
    [ eq (app "str.to_int" [ app "str.from_int" [ lv "#x" ] ]) (num 5.); eq (lv "#x") (num 6.) ]
    gx;
  chk "UF is a function" `Unsat
    [ eq (app "js.num2str" [ lv "#x" ]) (str "a"); eq (app "js.num2str" [ lv "#x" ]) (str "b") ]
    gx;
  chk "two UFs in one query" `Sat
    [
      eq (app "String.prototype.toLowerCase" [ lv "#s" ]) (str "a");
      app "decodeURIComponent.ok" [ lv "#s" ];
      eq (app "path.join/2" [ lv "#s"; str "b" ]) (str "a/b");
    ]
    gs;
  chk "Bool UF guard both sides (1)" `Sat [ app "js.tonumber.isnan" [ lv "#s" ] ] gs;
  chk "Bool UF guard both sides (2)" `Sat
    [ Expr.UnOp (Not, app "js.tonumber.isnan" [ lv "#s" ]) ]
    gs

let test_smt_numlit () =
  (* the SMT regex and the OCaml matcher agree (arguments are literals, not
     reduced before the solver) *)
  List.iter
    (fun (s, exp) ->
      let r = sat [ app "str.in_re.numlit" [ str s ] ] in
      Alcotest.check res (Printf.sprintf "smt numlit %S" s)
        (if exp then `Sat else `Unsat) r)
    numlit_samples;
  Alcotest.check res "numlit symbolic" `Sat
    (sat ~gamma:gs
       [ app "str.in_re.numlit" [ lv "#s" ]; eq (slen (lv "#s")) (num 3.) ]);
  Alcotest.check res "numlit with length and nondecimal" `Sat
    (sat ~gamma:gs
       [ app "str.in_re.numlit" [ lv "#s" ]; app "str.prefixof" [ str "0x"; lv "#s" ] ])

let test_smt_numeric () =
  let chk name exp fs gamma = Alcotest.check res name exp (sat ~gamma fs) in
  let u op e = Expr.UnOp (op, e) in
  chk "ToStringOp integer" `Sat [ eq (u ToStringOp (lv "#x")) (str "12"); eq (lv "#x") (num 12.) ] gx;
  chk "ToStringOp integer wrong" `Unsat [ eq (u ToStringOp (lv "#x")) (str "12"); eq (lv "#x") (num 13.) ] gx;
  chk "ToStringOp negative" `Unsat [ eq (u ToStringOp (lv "#x")) (str "5"); eq (lv "#x") (num (-5.)) ] gx;
  chk "ToStringOp negative ok" `Sat [ eq (u ToStringOp (lv "#x")) (str "-5"); eq (lv "#x") (num (-5.)) ] gx;
  chk "ToNumberOp digits" `Unsat [ eq (lv "#s") (str "42"); eq (u ToNumberOp (lv "#s")) (num 43.) ] gs;
  chk "ToNumberOp digits ok" `Sat [ eq (lv "#s") (str "42"); eq (u ToNumberOp (lv "#s")) (num 42.) ] gs;
  chk "ToNumberOp whitespace" `Unsat [ eq (lv "#s") (str " \t"); eq (u ToNumberOp (lv "#s")) (num 1.) ] gs;
  chk "M_floor" `Sat [ eq (u M_floor (lv "#x")) (num 1.); eq (lv "#x") (num 1.5) ] gx;
  chk "M_floor wrong" `Unsat [ eq (u M_floor (lv "#x")) (num 2.); eq (lv "#x") (num 1.5) ] gx;
  chk "M_ceil" `Unsat [ eq (u M_ceil (lv "#x")) (num 1.); eq (lv "#x") (num 1.5) ] gx;
  chk "M_abs" `Unsat [ eq (u M_abs (lv "#x")) (num (-1.)) ] gx;
  chk "M_sgn" `Unsat [ eq (u M_sgn (lv "#x")) (num 1.); eq (lv "#x") (num (-3.)) ] gx;
  chk "M_round" `Unsat [ eq (u M_round (lv "#x")) (num 2.); eq (lv "#x") (num 2.5) ] gx;
  chk "M_floor product" `Sat
    [
      eq (u M_floor (Expr.BinOp (lv "#r", FTimes, lv "#n"))) (num 1.);
      Expr.BinOp (num 0., FLessThanEqual, lv "#r");
      Expr.BinOp (lv "#r", FLessThan, num 1.);
    ]
    [ ("#r", Type.NumberType); ("#n", Type.NumberType) ];
  chk "ToUint32 in range" `Unsat [ eq (u ToUint32Op (lv "#x")) (num 6.); eq (lv "#x") (num 5.) ] gx;
  chk "ToUint32 out of range (UF)" `Sat [ eq (u ToUint32Op (lv "#x")) (num 1.); eq (lv "#x") (num (-1.)) ] gx;
  chk "ToInt32 in range" `Unsat [ eq (u ToInt32Op (lv "#x")) (num 5.); eq (lv "#x") (num (-5.)) ] gx;
  chk "ToUint16 in range" `Unsat [ eq (u ToUint16Op (lv "#x")) (num 1.); eq (lv "#x") (num 2.) ] gx;
  chk "FMod" `Unsat
    [ eq (Expr.BinOp (num (-7.), FMod, lv "#x")) (num 1.); eq (lv "#x") (num 2.) ]
    gx;
  chk "FMod ok" `Sat
    [ eq (Expr.BinOp (num (-7.), FMod, lv "#x")) (num (-1.)); eq (lv "#x") (num 2.) ]
    gx

let test_encoding_failure () =
  (* a non-finite literal left in a query is an encoding failure *)
  match sat ~gamma:gx [ eq (lv "#x") (num Float.nan) ] with
  | exception Smt.SMT_encoding_failure _ -> ()
  | _ -> Alcotest.fail "expected SMT_encoding_failure"

(* ------------------------------------------------------------------ *)
(* Reduction / typing                                                   *)
(* ------------------------------------------------------------------ *)

let test_reduction () =
  with_env @@ fun () ->
  let red e = Reduction.reduce_lexpr e in
  Alcotest.(check bool) "native on literals evaluated" true
    (Expr.equal (red (app "str.replace_all" [ str "abab"; str "b"; str "c" ])) (str "acac"));
  Alcotest.(check bool) "UF on literals kept" true
    (Expr.equal (red (app "js.num2str" [ num 1.5 ])) (app "js.num2str" [ num 1.5 ]));
  Alcotest.(check bool) "native on symbolic kept" true
    (match red (app "str.contains" [ lv "#s"; str "a" ]) with
    | FuncApp ("str.contains", _) -> true
    | _ -> false);
  Alcotest.(check bool) "numlit literal" true
    (Expr.equal (red (app "str.in_re.numlit" [ str " 0x1F " ])) (Expr.Lit (Bool true)))

let test_slen_fact () =
  with_env @@ fun () ->
  Config.servpips_semantics := true;
  let red e = Reduction.reduce_lexpr e in
  let is b e = Expr.equal (red e) (Expr.Lit (Bool b)) in
  let l = slen (lv "#s") in
  Alcotest.(check bool) "0 <= s-len" true (is true (Expr.BinOp (num 0., FLessThanEqual, l)));
  Alcotest.(check bool) "s-len < 0" true (is false (Expr.BinOp (l, FLessThan, num 0.)));
  Alcotest.(check bool) "-1 < s-len" true (is true (Expr.BinOp (num (-1.), FLessThan, l)));
  Alcotest.(check bool) "s-len = 1.5" true (is false (eq l (num 1.5)));
  Alcotest.(check bool) "s-len = -1" true (is false (eq l (num (-1.))));
  Alcotest.(check bool) "s-len = 2 kept" true (Expr.equal (red (eq l (num 2.))) (eq l (num 2.)))

let test_typing () =
  with_env @@ fun () ->
  let gamma = Type_env.init () in
  Type_env.update gamma "#s" Type.StringType;
  Type_env.update gamma "#x" Type.NumberType;
  let ty e = fst (Typing.type_lexpr gamma e) in
  let t = Alcotest.testable (Fmt.option Type.pp) (Option.equal Type.equal) in
  Alcotest.check t "substr : Str" (Some Type.StringType)
    (ty (app "str.substr" [ lv "#s"; lv "#x"; num 1. ]));
  Alcotest.check t "numlit : Bool" (Some Type.BooleanType) (ty (app "str.in_re.numlit" [ lv "#s" ]));
  Alcotest.check t "js.num2str : Str" (Some Type.StringType) (ty (app "js.num2str" [ lv "#x" ]));
  Alcotest.check t "ill-typed" None (ty (app "js.num2str" [ lv "#s" ]));
  (* reverse typing infers argument types *)
  let g2 = Type_env.init () in
  match Typing.reverse_type_lexpr true g2 [ (app "str.contains" [ lv "#a"; str "x" ], Type.BooleanType) ] with
  | Some g' ->
      Alcotest.(check (option (testable Type.pp Type.equal)))
        "reverse typing" (Some Type.StringType) (Type_env.get g' "#a")
  | None -> Alcotest.fail "reverse typing failed"

(* ------------------------------------------------------------------ *)
(* E7: NaN / Infinity in reductions                                     *)
(* ------------------------------------------------------------------ *)

let test_ieee_reduction () =
  with_env @@ fun () ->
  Config.servpips_semantics := true;
  let red e = Reduction.reduce_lexpr e in
  let is b e = Expr.equal (red e) (Expr.Lit (Bool b)) in
  let nan = num Float.nan and inf = num Float.infinity and ninf = num Float.neg_infinity in
  let lt a b = Expr.BinOp (a, FLessThan, b) and le a b = Expr.BinOp (a, FLessThanEqual, b) in
  Alcotest.(check bool) "NaN = NaN is false" true (is false (eq nan nan));
  Alcotest.(check bool) "NaN < 0.7 is false" true (is false (lt nan (num 0.7)));
  Alcotest.(check bool) "not (NaN < 0.7) is true" true (is true (Expr.UnOp (Not, lt nan (num 0.7))));
  Alcotest.(check bool) "not (0.7 <= NaN) is true" true (is true (Expr.UnOp (Not, le (num 0.7) nan)));
  Alcotest.(check bool) "false = (NaN < x) is true" true
    (is true (eq (Expr.Lit (Bool false)) (lt nan (lv "#x"))));
  Alcotest.(check bool) "x = NaN is false" true (is false (eq (lv "#x") nan));
  Alcotest.(check bool) "NaN <= x is false" true (is false (le nan (lv "#x")));
  Alcotest.(check bool) "x < Infinity (x finite)" true (is true (lt (lv "#x") inf));
  Alcotest.(check bool) "Infinity < x is false" true (is false (lt inf (lv "#x")));
  Alcotest.(check bool) "-Infinity <= x" true (is true (le ninf (lv "#x")));
  Alcotest.(check bool) "x = -Infinity is false" true (is false (eq (lv "#x") ninf));
  Alcotest.(check bool) "Infinity = Infinity" true (is true (eq inf inf));
  Alcotest.(check bool) "0 = -0" true (is true (eq (num 0.) (num (-0.))));
  Alcotest.(check bool) "not (x < 1) still rewritten" true
    (Expr.equal (red (Expr.UnOp (Not, lt (lv "#x") (num 1.)))) (le (num 1.) (lv "#x")));
  let ts e = Expr.UnOp (ToStringOp, e) in
  Alcotest.(check bool) "ToString(x) = \"5\" -> x = 5" true
    (Expr.equal (red (eq (ts (lv "#x")) (str "5"))) (eq (lv "#x") (num 5.)));
  Alcotest.(check bool) "ToString(x) = \"1.0\" is false" true (is false (eq (ts (lv "#x")) (str "1.0")));
  Alcotest.(check bool) "ToString(x) = \"1e21\" is false" true (is false (eq (ts (lv "#x")) (str "1e21")));
  Alcotest.(check bool) "ToString(x) = \"1e+21\" -> x = 1e21" true
    (Expr.equal (red (eq (ts (lv "#x")) (str "1e+21"))) (eq (lv "#x") (num 1e21)));
  Alcotest.(check (option bool)) "is_different ToString(x) \"5\"" None
    (Reduction.is_different [] (ts (lv "#x")) (str "5"));
  Alcotest.(check (option bool)) "is_different ToString(x) \"-5\"" None
    (Reduction.is_different [] (ts (lv "#x")) (str "-5"));
  Alcotest.(check (option bool)) "is_different ToString(x) \"a\"" (Some true)
    (Reduction.is_different [] (ts (lv "#x")) (str "a"));
  Alcotest.(check bool) "structural Literal.equal unchanged" true
    (Literal.equal (Num Float.nan) (Num Float.nan));
  Alcotest.(check bool) "Literal.ieee_equal NaN" false
    (Literal.ieee_equal (Num Float.nan) (Num Float.nan))

(* ------------------------------------------------------------------ *)
(* E16 / E8 / V1b: conformance with Node (servpips_conformance.json)    *)
(* ------------------------------------------------------------------ *)

let conformance =
  lazy
    (let candidates =
       (match Sys.getenv_opt "SERVPIPS_CONFORMANCE" with
       | Some f -> [ f ]
       | None -> [])
       @ [ "servpips_conformance.json"; "GillianCore/test/servpips_conformance.json" ]
     in
     match List.find_opt Sys.file_exists candidates with
     | Some f -> Yojson.Safe.from_file f
     | None -> Alcotest.fail "servpips_conformance.json not found")

let of_bits hex = Int64.float_of_bits (Int64.of_string ("0x" ^ hex))

let same_double a b =
  (Float.is_nan a && Float.is_nan b)
  || Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

let field name =
  match Lazy.force conformance with
  | `Assoc l -> (
      match List.assoc_opt name l with
      | Some (`List xs) -> xs
      | _ -> Alcotest.fail ("conformance field missing: " ^ name))
  | _ -> Alcotest.fail "bad conformance file"

let n2_samples =
  (* the 23 strings of experiment N2 (design 0.2), values from Node 20 *)
  [
    ("1_000", Float.nan); ("inf", Float.nan); ("nan", Float.nan); ("0x1p3", Float.nan);
    ("  12  ", 12.); ("0b11", 3.); ("0o7", 7.); ("1e400", Float.infinity);
    ("Infinity", Float.infinity); ("-Infinity", Float.neg_infinity); ("", 0.); (" ", 0.);
    ("1.", 1.); (".5", 0.5); ("+.5e1", 5.); ("0x", Float.nan); ("12abc", Float.nan);
    (" 12", 12.); ("1e", Float.nan); ("0X1F", 31.); ("-0x10", Float.nan);
    ("Infinityx", Float.nan); ("  -Infinity ", Float.neg_infinity);
  ]

let test_n2 () =
  List.iter
    (fun (s, v) ->
      let got = Arith_utils.js_string_to_number s in
      if not (same_double got v) then
        Alcotest.failf "js_string_to_number %S = %h, expected %h" s got v)
    n2_samples

let test_string_to_number () =
  let bad = ref 0 in
  List.iter
    (function
      | `List [ `String s; `String hex ] ->
          let exp = of_bits hex in
          let got = Arith_utils.js_string_to_number s in
          if not (same_double got exp) then (
            incr bad;
            if !bad <= 10 then
              Fmt.epr "StringToNumber %S: got %h expected %h@." s got exp);
          (* numlit (byte-level regex) agrees with Node on ASCII strings *)
          if String.for_all (fun c -> Char.code c < 128) s then
            if SF.numlit_matches s <> not (Float.is_nan exp) then (
              incr bad;
              if !bad <= 10 then Fmt.epr "numlit %S disagrees with Node@." s)
      | _ -> Alcotest.fail "bad string_to_number entry")
    (field "string_to_number");
  Alcotest.(check int) "StringToNumber / numlit mismatches" 0 !bad

let test_numlit_smt_vs_node () =
  (* the SMT regex on the first ASCII strings of the conformance data *)
  let bad = ref 0 and n = ref 0 in
  List.iter
    (function
      | `List [ `String s; `String hex ] when !n < 250 && String.for_all (fun c -> Char.code c < 128) s ->
          incr n;
          let exp = not (Float.is_nan (of_bits hex)) in
          let r = sat [ app "str.in_re.numlit" [ str s ] ] in
          if r <> (if exp then `Sat else `Unsat) then (
            incr bad;
            Fmt.epr "smt numlit %S disagrees with Node@." s)
      | _ -> ())
    (field "string_to_number");
  Alcotest.(check int) "SMT numlit mismatches" 0 !bad

let test_number_to_string () =
  let bad = ref 0 in
  List.iter
    (function
      | `List [ `String hex; `String exp ] ->
          let x = of_bits hex in
          let got = Arith_utils.js_number_to_string x in
          if got <> exp then (
            incr bad;
            if !bad <= 10 then Fmt.epr "Number::toString %h: got %s expected %s@." x got exp)
      | _ -> Alcotest.fail "bad number_to_string entry")
    (field "number_to_string");
  Alcotest.(check int) "Number::toString mismatches" 0 !bad

let test_unary name (f : float -> float) ~exact_zero () =
  let bad = ref 0 in
  List.iter
    (function
      | `List [ `String hx; `String hr ] ->
          let x = of_bits hx and exp = of_bits hr in
          let got = f x in
          let ok = if exact_zero then same_double got exp else same_double got exp || got = exp in
          if not ok then (
            incr bad;
            if !bad <= 10 then Fmt.epr "%s %h: got %h expected %h@." name x got exp)
      | _ -> Alcotest.fail "bad unary entry")
    (field name);
  Alcotest.(check int) (name ^ " mismatches") 0 !bad

let unop op x =
  Config.servpips_semantics := true;
  match Engine.CExprEval.evaluate_unop op (Literal.Num x) with
  | Literal.Num r -> r
  | _ -> Float.nan

let test_fmod () =
  let bad = ref 0 in
  List.iter
    (function
      | `List [ `String ha; `String hb; `String hr ] ->
          let a = of_bits ha and b = of_bits hb and exp = of_bits hr in
          let got = Float.rem a b in
          if not (same_double got exp) then (
            incr bad;
            if !bad <= 10 then Fmt.epr "fmod %h %h: got %h expected %h@." a b got exp)
      | _ -> Alcotest.fail "bad fmod entry")
    (field "fmod");
  Alcotest.(check int) "fmod mismatches" 0 !bad

(* ------------------------------------------------------------------ *)
(* E19: PFS hash index = list semantics                                 *)
(* ------------------------------------------------------------------ *)

module PFS = Engine.PFS

let test_ext_list_remove_duplicates () =
  let l = Utils.Ext_list.of_list [ 1; 1; 1; 2; 1; 2 ] in
  Utils.Ext_list.remove_duplicates l;
  Alcotest.(check (list int)) "remove_duplicates" [ 1; 2 ] (Utils.Ext_list.to_list l);
  Alcotest.(check int) "length" 2 (Utils.Ext_list.length l);
  Utils.Ext_list.append 3 l;
  Alcotest.(check (list int)) "append after remove_duplicates" [ 1; 2; 3 ]
    (Utils.Ext_list.to_list l)

let test_pfs_index () =
  Config.servpips_semantics := true;
  Random.init 42;
  let atoms =
    Array.init 12 (fun i ->
        match i mod 4 with
        | 0 -> eq (lv ("#a" ^ string_of_int i)) (num (float_of_int i))
        | 1 -> Expr.BinOp (lv "#x", FLessThan, num (float_of_int i))
        | 2 -> eq (lv "#s") (str (string_of_int i))
        | _ -> Expr.UnOp (Not, eq (lv "#y") (num (float_of_int i))))
  in
  let pick () = atoms.(Random.int (Array.length atoms)) in
  let check pfs =
    Array.iter
      (fun f ->
        let expected = List.exists (Expr.equal f) (PFS.to_list pfs) in
        if PFS.mem pfs f <> expected then Alcotest.failf "PFS.mem disagrees on %a" Expr.pp f)
      atoms
  in
  for _round = 1 to 200 do
    let pfs = PFS.init () in
    for _step = 1 to 30 do
      (match Random.int 9 with
      | 0 | 1 | 2 -> PFS.extend pfs (pick ())
      | 3 -> PFS.filter (fun e -> not (Expr.equal e (pick ()))) pfs
      | 4 ->
          PFS.map_inplace
            (fun e -> if Random.bool () then e else pick ())
            pfs
      | 5 ->
          let c = PFS.copy pfs in
          PFS.extend c (pick ());
          check c
      | 6 -> PFS.merge_into_left pfs (PFS.of_list [ pick (); pick () ])
      | 7 -> PFS.subst_expr_for_expr (lv "#x") (lv "#y") pfs
      | _ -> PFS.remove_duplicates pfs);
      check pfs
    done
  done

(* ---- round 2 (perf): stamps, typing without commits, negate, alias ---- *)

let test_stamps () =
  Config.servpips_semantics := true;
  let p = PFS.of_list [ eq (lv "#a") (num 1.) ] in
  let g0 = PFS.generation p in
  let q = PFS.copy p in
  Alcotest.(check int) "copy keeps the stamp" g0 (PFS.generation q);
  PFS.extend q (eq (lv "#b") (num 2.));
  Alcotest.(check bool) "extend renews the stamp" true (PFS.generation q <> g0);
  Alcotest.(check int) "the original is untouched" g0 (PFS.generation p);
  PFS.extend p (eq (lv "#a") (num 1.));
  Alcotest.(check int) "extending with a present formula keeps it" g0
    (PFS.generation p);
  let r = PFS.of_list [ eq (lv "#a") (num 1.) ] in
  Alcotest.(check bool) "a new set has its own stamp" true
    (PFS.generation r <> g0);
  let t = Type_env.init () in
  Type_env.update t "#a" Type.NumberType;
  let s0 = Type_env.generation t in
  let t' = Type_env.copy t in
  Alcotest.(check int) "env copy keeps the stamp" s0 (Type_env.generation t');
  Type_env.update t' "#a" Type.NumberType;
  Alcotest.(check int) "same binding keeps it" s0 (Type_env.generation t');
  Type_env.update t' "#b" Type.StringType;
  Alcotest.(check bool) "new binding renews it" true
    (Type_env.generation t' <> s0);
  Type_env.remove t "#zz";
  Alcotest.(check int) "removing an absent variable keeps it" s0
    (Type_env.generation t)

let test_typing_no_commit () =
  Config.servpips_semantics := true;
  let gamma = Type_env.init () in
  let t, ok = Typing.type_lexpr gamma (Expr.UnOp (ToNumberOp, lv "#n")) in
  Alcotest.(check bool) "typable" true ok;
  Alcotest.(check bool) "number" true (t = Some Type.NumberType);
  Alcotest.(check bool) "#n : Str not added to gamma" true
    (Type_env.get gamma "#n" = None);
  (* reverse typing infers nothing from a disjunct *)
  let f =
    Expr.BinOp
      ( eq (lv "#u") (lv "#s"),
        Or,
        Expr.BinOp (Expr.UnOp (ToNumberOp, lv "#n"), FLessThan, num 3.) )
  in
  match Typing.reverse_type_lexpr true gamma [ (f, Type.BooleanType) ] with
  | Some g -> Alcotest.(check bool) "no #n from the disjunct" true (Type_env.get g "#n" = None)
  | None -> Alcotest.fail "disjunction not typable"

let test_negate_nan () =
  Config.servpips_semantics := true;
  let nan = num Float.nan in
  let a = Expr.BinOp (nan, FLessThan, lv "#x") in
  Alcotest.(check bool) "not (NaN < x) stays a negation" true
    (match Expr.negate a with
    | Expr.UnOp (Not, BinOp (Lit (Num f), FLessThan, LVar "#x")) ->
        Float.is_nan f
    | _ -> false);
  let b = Expr.BinOp (num 1., FLessThan, lv "#x") in
  Alcotest.(check bool) "not (1 < x) is x <= 1" true
    (Expr.negate b = Expr.BinOp (lv "#x", FLessThanEqual, num 1.))

let test_input_not_loc () =
  Config.servpips_semantics := true;
  let saved = !Reduction.servpips_input_not_loc in
  Reduction.servpips_input_not_loc := (fun x l -> x = "#in" && l <> "#mine");
  let r e = Reduction.reduce_lexpr e in
  Alcotest.(check bool) "input vs other object" true
    (r (eq (lv "#in") (Expr.ALoc "#other")) = Expr.false_);
  Alcotest.(check bool) "input vs concrete location" true
    (r (eq (Expr.Lit (Loc "$lg")) (lv "#in")) = Expr.false_);
  Alcotest.(check bool) "input vs its own location stays" true
    (r (eq (lv "#in") (Expr.ALoc "#mine")) <> Expr.false_);
  Alcotest.(check bool) "other variable stays" true
    (r (eq (lv "#y") (Expr.ALoc "#other")) <> Expr.false_);
  Reduction.servpips_input_not_loc := saved

let test_msgn_copysign () =
  (* the JSIL runtime (i__sameValue, Math.min/max) tells -0 from +0 with
     M_sgn: it must stay copysign(1, x) under SERVPIPS semantics *)
  let chk x exp =
    Alcotest.(check (float 0.)) (Fmt.str "M_sgn %h" x) exp (unop M_sgn x)
  in
  chk 0. 1.;
  chk (-0.) (-1.);
  chk 5. 1.;
  chk (-5.) (-1.);
  chk Float.infinity 1.;
  chk Float.neg_infinity (-1.)

let tests : unit Alcotest.test_case list =
  [
    ("builtin table", `Quick, test_table);
    ("is_boolean_expr hook", `Quick, test_is_boolean);
    ("eval_concrete natives", `Quick, test_eval_concrete);
    ("numlit concrete", `Quick, test_numlit_concrete);
    ("smt builtins", `Quick, test_smt_builtins);
    ("smt numlit regex = concrete", `Quick, test_smt_numlit);
    ("smt numeric encodings", `Quick, test_smt_numeric);
    ("smt non-finite literal", `Quick, test_encoding_failure);
    ("reduction of builtins", `Quick, test_reduction);
    ("typing of builtins", `Quick, test_typing);
    ("s-len >= 0 fact", `Quick, test_slen_fact);
    ("E7 IEEE reductions", `Quick, test_ieee_reduction);
    ("E16 StringToNumber N2 samples", `Quick, test_n2);
    ("V1b StringToNumber + numlit vs Node", `Quick, test_string_to_number);
    ("V1b SMT numlit vs Node", `Quick, test_numlit_smt_vs_node);
    ("V1b Number::toString vs Node", `Quick, test_number_to_string);
    ("V1b ToInt32 vs Node", `Quick, test_unary "to_int32" Arith_utils.to_int32 ~exact_zero:false);
    ("V1b ToUint32 vs Node", `Quick, test_unary "to_uint32" Arith_utils.to_uint32 ~exact_zero:false);
    ("V1b ToUint16 vs Node", `Quick, test_unary "to_uint16" Arith_utils.to_uint16 ~exact_zero:false);
    ("V1b Math.floor vs Node", `Quick, test_unary "floor" (unop M_floor) ~exact_zero:true);
    ("V1b Math.ceil vs Node", `Quick, test_unary "ceil" (unop M_ceil) ~exact_zero:true);
    ("V1b Math.round vs Node", `Quick, test_unary "round" (unop M_round) ~exact_zero:true);
    ("V1b Math.sign (Arith_utils.js_sign) vs Node", `Quick, test_unary "sign" Arith_utils.js_sign ~exact_zero:true);
    ("M_sgn is copysign(1, x) (R2)", `Quick, test_msgn_copysign);
    ("V1b Math.abs vs Node", `Quick, test_unary "abs" (unop M_abs) ~exact_zero:true);
    ("V1b % (fmod) vs Node", `Quick, test_fmod);
    ("Ext_list.remove_duplicates keeps the list consistent", `Quick, test_ext_list_remove_duplicates);
    ("E19 PFS index = list", `Quick, test_pfs_index);
    ("E19 PFS / Type_env stamps", `Quick, test_stamps);
    ("typing does not commit inferred types", `Quick, test_typing_no_commit);
    ("E7 negate keeps NaN comparisons", `Quick, test_negate_nan);
    ("input never equals another location", `Quick, test_input_not_loc);
  ]

(* SERVPIPS engine-core unit tests (WP1): builtin function table (E5), SMT
   encodings (E5, E6), NaN semantics (E7), StringToNumber (E16),
   Number::toString (E8), PFS index (E19). *)

open Gil_syntax
module SF = Smt.Servpips_functions
module Reduction = Engine.Reduction
module Typing = Engine.Typing
module Type_env = Engine.Type_env

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
  ]

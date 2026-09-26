(* SERVPIPS mode: JSONL event output. See servpips.mli. *)

type config = {
  log_path : string;
  runtime_dir : string option;
  shard_json : Yojson.Safe.t;
  smt_timeout_ms : int;
}

let default_log_path = "events.jsonl"

let default_config () =
  {
    log_path = default_log_path;
    runtime_dir = None;
    shard_json = `Null;
    smt_timeout_ms = Option.value (Smt.timeout_ms ()) ~default:(-1);
  }

let enabled_ref = ref false
let config_ref : config option ref = ref None
let chan : out_channel option ref = ref None
let enabled () = !enabled_ref

let config () =
  match !config_ref with
  | Some c -> c
  | None -> default_config ()

let enable (c : config) =
  (match !chan with
  | Some oc -> close_out_noerr oc
  | None -> ());
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] 0o644
      c.log_path
  in
  chan := Some oc;
  config_ref := Some c;
  enabled_ref := true

(* Non-finite floats are not JSON: encode them as {"nonfinite": ...}. *)
let rec encode_nonfinite (j : Yojson.Safe.t) : Yojson.Safe.t =
  match j with
  | `Float f when Float.is_nan f -> `Assoc [ ("nonfinite", `String "NaN") ]
  | `Float f when f = Float.infinity ->
      `Assoc [ ("nonfinite", `String "Infinity") ]
  | `Float f when f = Float.neg_infinity ->
      `Assoc [ ("nonfinite", `String "-Infinity") ]
  | `List l -> `List (List.map encode_nonfinite l)
  | `Assoc l -> `Assoc (List.map (fun (k, v) -> (k, encode_nonfinite v)) l)
  | `Tuple l -> `Tuple (List.map encode_nonfinite l)
  | `Variant (s, Some v) -> `Variant (s, Some (encode_nonfinite v))
  | j -> j

let emit (j : Yojson.Safe.t) : unit =
  if !enabled_ref then
    match !chan with
    | None -> ()
    | Some oc ->
        output_string oc (Yojson.Safe.to_string (encode_nonfinite j));
        output_char oc '\n';
        flush oc

let expr_json (e : Expr.t) : Yojson.Safe.t =
  `Assoc
    [
      ("pp", `String (Fmt.to_to_string Expr.pp e));
      ("ast", encode_nonfinite (Expr.to_yojson e));
    ]

let pc_json (pc : Expr.t list) : Yojson.Safe.t = `List (List.map expr_json pc)

let types_json (types : (Expr.t * Type.t) list) : Yojson.Safe.t =
  `List (List.map (fun (e, t) -> `List [ expr_json e; `String (Type.str t) ]) types)

exception Path_end of { status : string; reason : string }

let () =
  Printexc.register_printer (function
    | Path_end { status; reason } ->
        Some (Printf.sprintf "Servpips.Path_end(%s, %S)" status reason)
    | _ -> None)

let opt_string = function
  | Some s -> `String s
  | None -> `Null

let note ~code ~msg ?site ?data () =
  emit
    (`Assoc
       [
         ("ev", `String "note");
         ("code", `String code);
         ("msg", `String msg);
         ("site", opt_string site);
         ("data", Option.value data ~default:`Null);
       ])

let fork_commit () =
  match Sys.getenv_opt "SERVPIPS_FORK_COMMIT" with
  | Some s when s <> "" -> s
  | _ -> "unknown"

let hello_builtins : (unit -> Yojson.Safe.t) ref = ref (fun () -> `Assoc [])

let hello ~unroll () =
  if !enabled_ref then
    let c = config () in
    let z3 = try Smt.solver_version () with _ -> "unknown" in
    emit
      (`Assoc
         [
           ("ev", `String "hello");
           ("v", `Int 2);
           ("fork", `String (fork_commit ()));
           ("z3", `String z3);
           ("shard", c.shard_json);
           ("unroll", `Int unroll);
           ("smt_timeout_ms", `Int c.smt_timeout_ms);
           ("builtins", !hello_builtins ());
         ])

let emit_end ~status ~reason ?outcome ~pc ~types () =
  emit
    (`Assoc
       [
         ("ev", `String "end");
         ("status", `String status);
         ("reason", `String reason);
         ("outcome", opt_string outcome);
         ("pc", pc_json pc);
         ("types", types_json types);
       ])

let pc_and_types_of_asrt (a : Asrt.t) =
  let pc =
    List.filter_map
      (function
        | Asrt.Pure f -> Some f
        | _ -> None)
      a
  in
  let types =
    List.concat_map
      (function
        | Asrt.Types l -> l
        | _ -> [])
      a
  in
  (pc, types)

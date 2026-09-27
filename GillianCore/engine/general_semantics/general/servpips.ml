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
let start_sampler_ref : (unit -> unit) ref = ref (fun () -> ())
let start_time = ref (Unix.gettimeofday ())
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
  enabled_ref := true;
  start_time := Unix.gettimeofday ();
  Config.servpips_semantics := true;
  Smt.servpips_enable ();
  !start_sampler_ref ()

(* [gillian-js compile --servpips]: compile exactly as [wpst --servpips]
   would (same preamble, same SERVPIPS compilation), without an event log. *)
let enable_compile ~runtime_dir =
  (match !chan with
  | Some oc -> close_out_noerr oc
  | None -> ());
  chan := None;
  config_ref := Some { (default_config ()) with runtime_dir };
  enabled_ref := true;
  Config.servpips_semantics := true

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
  (* sorted, for deterministic output *)
  let types = List.sort (fun (e1, _) (e2, _) -> Expr.compare e1 e2) types in
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

let hello_builtins : (unit -> Yojson.Safe.t) ref =
  ref Smt.Servpips_functions.hello_json

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

(* ------------------------------------------------------------------------ *)
(* Path accounting (E1, E3, E9, E13)                                         *)
(* ------------------------------------------------------------------------ *)

let end_statuses =
  [ "returned"; "threw"; "truncated"; "unknown"; "unsupported"; "error" ]

type counters = {
  mutable leaves : int;
  ends : (string, int) Hashtbl.t;
  mutable infeasible : int;
  mutable vanished : int;
  mutable prunes : int;
  mutable prunes_dup : int;
  mutable max_branch : int;
  mutable unknown_assumed_sat : int;
  mutable entail_unknown : int;
  mutable encode_failures : int;
  mutable internal_exceptions : int;
  mutable fatal : string option;
  mutable stats_emitted : bool;
}

let counters =
  {
    leaves = 0;
    ends = Hashtbl.create 8;
    infeasible = 0;
    vanished = 0;
    prunes = 0;
    prunes_dup = 0;
    max_branch = 0;
    unknown_assumed_sat = 0;
    entail_unknown = 0;
    encode_failures = 0;
    internal_exceptions = 0;
    fatal = None;
    stats_emitted = false;
  }

let incr_end status =
  let n = Option.value (Hashtbl.find_opt counters.ends status) ~default:0 in
  Hashtbl.replace counters.ends status (n + 1)

(* One line, no decoration: newlines/tabs become spaces, runs of spaces and
   of '!' (Gillian's failure banners) are collapsed, then truncated. *)
let truncate_reason ?(max = 2000) s =
  let b = Buffer.create (String.length s) in
  let last = ref ' ' in
  String.iter
    (fun c ->
      let c = match c with '\n' | '\r' | '\t' -> ' ' | c -> c in
      if (c = ' ' || c = '!') && !last = c then () else Buffer.add_char b c;
      last := c)
    s;
  let s = String.trim (Buffer.contents b) in
  if String.length s <= max then s else String.sub s 0 max ^ "...(truncated)"

let record_end ~status ~reason ?outcome ~pc ~types () =
  let status, reason =
    if List.mem status end_statuses then (status, reason)
    else ("error", Printf.sprintf "invalid end status %S: %s" status reason)
  in
  let reason = truncate_reason reason in
  counters.leaves <- counters.leaves + 1;
  incr_end status;
  emit_end ~status ~reason ?outcome ~pc ~types ()

let record_truncated ~pc ~types () =
  counters.max_branch <- counters.max_branch + 1;
  record_end ~status:"truncated" ~reason:"max_branching" ~pc ~types ()

let record_infeasible () =
  counters.leaves <- counters.leaves + 1;
  counters.infeasible <- counters.infeasible + 1

let record_vanished ~detail ~pc ~types () =
  counters.vanished <- counters.vanished + 1;
  note ~code:"vanished" ~msg:(truncate_reason detail) ();
  record_end ~status:"error" ~reason:"vanished" ~pc ~types ()

(* A one-line description of an exception (for [end.reason] and notes). *)
let exn_msg (e : exn) : string =
  let raw =
    match e with
    | Gillian_result.Exc.Gillian_error (AnalysisFailures fs) ->
        String.concat "; "
          (List.map (fun (f : Gillian_result.Error.analysis_failure) -> f.msg) fs)
    | Gillian_result.Exc.Gillian_error err -> Gillian_result.Error.show_brief err
    | Gillian_result.Exc.Gillian_internal_error { msg; _ } ->
        "internal error: " ^ msg
    | Failure msg -> "Failure: " ^ msg
    | e -> Printexc.to_string e
  in
  String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) raw

let internal_exception ~msg =
  counters.internal_exceptions <- counters.internal_exceptions + 1;
  note ~code:"internal-exception" ~msg:(truncate_reason msg) ()

exception Assume_failed of Expr.t

let () =
  Printexc.register_printer (function
    | Assume_failed e ->
        Some (Fmt.str "Servpips.Assume_failed(%a)" Expr.pp e)
    | _ -> None)

exception Path_end_outcome of {
  status : string;
  reason : string;
  outcome : string option;
}

let end_path ~status ~reason ?outcome () =
  raise (Path_end_outcome { status; reason; outcome })

(* Which engine component took the last negative decision (a formula found
   unsatisfiable / a branch side dropped). Set by the symbolic state. *)
let last_decision = ref "solver"
let set_decision (by : string) = last_decision := by

(* prune events, deduplicated on their exact content (structural key, hashed
   over every component so that keys sharing a prefix do not collide) *)
module Prune_key = struct
  type t = Expr.t * Expr.t * string * string * Expr.t list * (Expr.t * Type.t) list

  let equal (a : t) (b : t) = compare a b = 0

  let hash ((g, go, kept, by, pc, types) : t) =
    let mix h x = (h * 65599) + Hashtbl.hash x in
    let h = List.fold_left mix (mix (mix 17 g) go) pc in
    let h = List.fold_left mix h types in
    mix h (kept, by) land max_int
end

module Prune_tbl = Hashtbl.Make (Prune_key)

let prune_seen : unit Prune_tbl.t = Prune_tbl.create 1024

(* The part of a path condition relevant to a set of variables: the
   conjuncts connected to [vars] through shared logical variables or abstract
   locations (transitively), plus every conjunct without variables. *)
let pc_slice ~(vars : SS.t) (pc : Expr.t list) (types : (Expr.t * Type.t) list)
    =
  let info e = SS.union (Expr.lvars e) (Expr.alocs e) in
  let pcs = List.map (fun e -> (e, info e)) pc in
  let rec fix rel =
    let rel' =
      List.fold_left
        (fun acc (_, i) -> if SS.disjoint i rel then acc else SS.union acc i)
        rel pcs
    in
    if SS.equal rel' rel then rel else fix rel'
  in
  let rel = fix vars in
  let pc' =
    List.filter_map
      (fun (e, i) ->
        if SS.is_empty i || not (SS.disjoint i rel) then Some e else None)
      pcs
  in
  let types' =
    List.filter
      (fun ((e : Expr.t), _) ->
        match e with
        | LVar x | ALoc x -> SS.mem x rel
        | _ -> false)
      types
    |> List.sort (fun (e1, _) (e2, _) -> Expr.compare e1 e2)
  in
  (pc', types')

let record_prune ~guard ~guard_orig ~kept ~by ~pc ~types () =
  if !enabled_ref then
    let vars =
      List.fold_left SS.union SS.empty
        [
          Expr.lvars guard; Expr.alocs guard; Expr.lvars guard_orig;
          Expr.alocs guard_orig;
        ]
    in
    let pc, types = pc_slice ~vars pc types in
    let key = (guard, guard_orig, kept, by, pc, types) in
    if Prune_tbl.mem prune_seen key then
      counters.prunes_dup <- counters.prunes_dup + 1
    else (
      Prune_tbl.replace prune_seen key ();
      counters.prunes <- counters.prunes + 1;
      emit
        (`Assoc
          [
            ("ev", `String "prune");
            ("guard", expr_json guard);
            ("guard_orig", expr_json guard_orig);
            ("kept", `String kept);
            ("by", `String by);
            ("pc", pc_json pc);
            ("types", types_json types);
          ]))

let note_unknown ~entailment =
  if entailment then (
    counters.entail_unknown <- counters.entail_unknown + 1;
    note ~code:"entail-unknown"
      ~msg:"entailment query answered unknown: treated as not entailed" ())
  else (
    counters.unknown_assumed_sat <- counters.unknown_assumed_sat + 1;
    note ~code:"unknown-assumed-sat"
      ~msg:"satisfiability query answered unknown: treated as satisfiable" ())

let encode_failure ~msg =
  counters.encode_failures <- counters.encode_failures + 1;
  raise
    (Path_end
       {
         status = "unsupported";
         reason = truncate_reason ("smt-encoding: " ^ msg);
       })

let fatal () = counters.fatal

let set_fatal msg =
  match counters.fatal with
  | None -> counters.fatal <- Some (truncate_reason msg)
  | Some _ -> ()

let rss_mb () =
  try
    let ic = open_in "/proc/self/status" in
    let rec loop () =
      match input_line ic with
      | line ->
          if String.length line > 6 && String.sub line 0 6 = "VmHWM:" then
            let v =
              Scanf.sscanf
                (String.sub line 6 (String.length line - 6))
                " %d" (fun x -> x)
            in
            Some v
          else loop ()
      | exception End_of_file -> None
    in
    let r = loop () in
    close_in_noerr ic;
    match r with
    | Some kb -> `Int (kb / 1024)
    | None -> `Null
  with _ -> `Null

let emit_stats () =
  if !enabled_ref && not counters.stats_emitted then (
    counters.stats_emitted <- true;
    let ends =
      List.map
        (fun s ->
          (s, `Int (Option.value (Hashtbl.find_opt counters.ends s) ~default:0)))
        end_statuses
    in
    emit
      (`Assoc
         [
           ("ev", `String "stats");
           ("leaves", `Int counters.leaves);
           ("ends", `Assoc ends);
           ("infeasible", `Int counters.infeasible);
           ("vanished", `Int counters.vanished);
           ("prunes", `Int counters.prunes);
           ("max_branch", `Int counters.max_branch);
           ( "solver",
             `Assoc
               [
                 ("queries", `Int !Smt.query_count);
                 ("unknown_assumed_sat", `Int counters.unknown_assumed_sat);
                 ("entail_unknown", `Int counters.entail_unknown);
                 ("encode_failures", `Int counters.encode_failures);
               ] );
           ( "fatal",
             match counters.fatal with
             | None -> `Null
             | Some s -> `String s );
           ("seconds", `Float (Unix.gettimeofday () -. !start_time));
           ("rss_mb", rss_mb ());
         ]))

(* Sampling profiler (diagnostics only; never changes results). *)
let sampling = ref false
let sample_hook : (unit -> string) ref = ref (fun () -> "")

let current_rss_mb () =
  try
    let ic = open_in "/proc/self/statm" in
    let r = Scanf.sscanf (input_line ic) "%d %d" (fun _ rss -> rss) in
    close_in_noerr ic;
    r * 4096 / 1048576
  with _ -> -1

let start_sampler () =
  match Sys.getenv_opt "SERVPIPS_SAMPLE" with
  | None | Some "" -> ()
  | Some spec ->
      let file, ms =
        match String.rindex_opt spec ':' with
        | Some i -> (
            let f = String.sub spec 0 i in
            let t = String.sub spec (i + 1) (String.length spec - i - 1) in
            match int_of_string_opt t with
            | Some ms when ms > 0 -> (f, ms)
            | _ -> (spec, 10))
        | None -> (spec, 10)
      in
      let oc = open_out file in
      let n = ref 0 in
      let t0 = Unix.gettimeofday () in
      sampling := true;
      Sys.set_signal Sys.sigprof
        (Sys.Signal_handle
           (fun _ ->
             incr n;
             let line =
               try !sample_hook () with e -> "exn " ^ Printexc.to_string e
             in
             Printf.fprintf oc "%d\t%.2f\t%d\t%s\n" !n
               (Unix.gettimeofday () -. t0)
               (current_rss_mb ()) line;
             flush oc));
      let iv = float_of_int ms /. 1000. in
      ignore
        (Unix.setitimer Unix.ITIMER_PROF
           { Unix.it_interval = iv; Unix.it_value = iv })

let () = start_sampler_ref := start_sampler

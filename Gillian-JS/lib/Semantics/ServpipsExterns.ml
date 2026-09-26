(* Registry of SERVPIPS externs. See ServpipsExterns.mli. *)

module Expr = Gillian.Gil_syntax.Expr
module Literal = Gillian.Gil_syntax.Literal
module Asrt = Gillian.Gil_syntax.Asrt
module Type = Gillian.Gil_syntax.Type

module type ENV = sig
  type st
  type vt

  module Val : Gillian.General.Val.S with type t = vt

  module State :
    Gillian.General.State.S with type t = st and type vt = vt

  val symbolic : bool
  val extern : string
  val proc : string
  val idx : int
end

type ('st, 'vt) outcome = Return of 'st * 'vt | Throw of 'st * 'vt

type handler = {
  run :
    'st 'vt.
    (module ENV with type st = 'st and type vt = 'vt) ->
    'st ->
    'vt list ->
    ('st, 'vt) outcome list;
}

let prefix = "servpips_"
let is_servpips_extern name = String.starts_with ~prefix name
let table : (string, handler) Hashtbl.t = Hashtbl.create 32

let register name h =
  if not (is_servpips_extern name) then
    invalid_arg
      (Printf.sprintf "ServpipsExterns.register: %S does not start with %S"
         name prefix);
  Hashtbl.replace table name h

let find name = Hashtbl.find_opt table name

let registered () =
  Hashtbl.fold (fun k _ acc -> k :: acc) table [] |> List.sort String.compare

(* ------------------------------------------------------------------------ *)
(* Built-in: servpips_echo(v, ...) returns v.                                *)
(* ------------------------------------------------------------------------ *)

let echo : handler =
  {
    run =
      (fun (type st vt)
           (module E : ENV with type st = st and type vt = vt)
           (state : st)
           (args : vt list) ->
        match args with
        | v :: _ -> [ Return (state, v) ]
        | [] -> [ Return (state, E.Val.from_literal Literal.Undefined) ]);
  }

(* ------------------------------------------------------------------------ *)
(* Built-in: servpips_log(tag, v1, ..., vn), the prototype's __servpips_log. *)
(* Appends one JSON line per configuration to $SERVPIPS_LOG (default         *)
(* servpips_log.jsonl): the symbolic argument values (objects are walked     *)
(* through the JS memory actions GetAllProps/GetCell), the path condition    *)
(* and the type environment.                                                 *)
(* ------------------------------------------------------------------------ *)

let log_chan =
  lazy
    (let path =
       Option.value (Sys.getenv_opt "SERVPIPS_LOG") ~default:"servpips_log.jsonl"
     in
     open_out_gen [ Open_append; Open_creat; Open_wronly ] 0o666 path)

let log_expr_json (e : Expr.t) : Yojson.Safe.t =
  `Assoc
    [ ("pp", `String (Fmt.to_to_string Expr.pp e)); ("ast", Expr.to_yojson e) ]

let log : handler =
  {
    run =
      (fun (type st vt)
           (module E : ENV with type st = st and type vt = vt)
           (state : st)
           (v_args : vt list) ->
        let rec serialise depth (v : vt) : Yojson.Safe.t =
          let e = E.Val.to_expr v in
          let is_loc =
            match e with
            | Expr.Lit (Loc _) | Expr.ALoc _ -> true
            | _ -> false
          in
          if (not is_loc) || depth > 8 then log_expr_json e
          else
            try
              match E.State.execute_action "GetAllProps" state [ v ] with
              | [ Ok (_, [ _; props ]) ] -> (
                  match E.Val.to_list props with
                  | Some props ->
                      let fields =
                        List.filter_map
                          (fun p ->
                            match E.Val.to_literal p with
                            | Some (String pname)
                              when String.length pname > 0 && pname.[0] <> '@'
                              -> (
                                match
                                  E.State.execute_action "GetCell" state [ v; p ]
                                with
                                | [ Ok (_, [ _; _; ffv ]) ] -> (
                                    match E.Val.to_list ffv with
                                    | Some (_ :: value :: _) ->
                                        Some (pname, serialise (depth + 1) value)
                                    | _ ->
                                        Some
                                          (pname, log_expr_json (E.Val.to_expr ffv))
                                    )
                                | _ ->
                                    Some (pname, `String "<GetCell: branching>"))
                            | _ -> None)
                          props
                      in
                      `Assoc
                        [ ("loc", log_expr_json e); ("object", `Assoc fields) ]
                  | None -> log_expr_json e)
              | _ ->
                  `Assoc
                    [ ("loc", log_expr_json e); ("object", `String "<unknown>") ]
            with exn ->
              `Assoc
                [
                  ("loc", log_expr_json e);
                  ( "object",
                    `String ("<error: " ^ Printexc.to_string exn ^ ">") );
                ]
        in
        let asrt =
          E.State.to_assertions ~to_keep:Containers.SS.empty state
        in
        let pure =
          List.filter_map
            (function
              | Asrt.Pure f -> Some f
              | _ -> None)
            asrt
        in
        let types =
          List.concat_map
            (function
              | Asrt.Types l -> l
              | _ -> [])
            asrt
        in
        let tag, rest =
          match v_args with
          | t :: rest -> (Fmt.to_to_string E.Val.pp t, rest)
          | [] -> ("", [])
        in
        let json =
          `Assoc
            [
              ("tag", `String tag);
              ("proc", `String E.proc);
              ("args", `List (List.map (serialise 0) rest));
              ("pc", `List (List.map log_expr_json pure));
              ( "types",
                `List
                  (List.map
                     (fun (e, t) ->
                       `List
                         [ `String (Fmt.to_to_string Expr.pp e); `String (Type.str t) ])
                     types) );
            ]
        in
        let oc = Lazy.force log_chan in
        output_string oc (Yojson.Safe.to_string json);
        output_char oc '\n';
        flush oc;
        [ Return (state, E.Val.from_literal Literal.Undefined) ]);
  }

let () =
  register "servpips_echo" echo;
  register "servpips_log" log

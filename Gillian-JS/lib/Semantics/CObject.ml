open Gillian.Concrete
module Var = Gillian.Gil_syntax.Var

(* [seq]: SERVPIPS (E15) creation sequence number of every property,
   maintained and used by [properties] under the SERVPIPS semantics
   (exec --servpips) only. *)
type t = { props : (string, Values.t) Hashtbl.t; seq : (string, int) Hashtbl.t }

let pp fmt (loc, obj, metadata) =
  let pp_kv fmt (prop, prop_val) =
    Fmt.pf fmt "%s: %a" prop Values.pp prop_val
  in
  Fmt.pf fmt "@[<h>%s|-> [ %a ], %a@]" loc
    (Fmt.hashtbl ~sep:Fmt.comma pp_kv)
    obj.props Values.pp metadata

let init () : t =
  {
    props = Hashtbl.create Config.medium_tbl_size;
    seq = Hashtbl.create Config.medium_tbl_size;
  }

let get (obj : t) (prop : string) = Hashtbl.find_opt obj.props prop

(* global, monotonic: only the relative order within an object matters *)
let seq_counter = ref 0

let set (obj : t) (prop : string) (value : Values.t) =
  if !Config.servpips_semantics && not (Hashtbl.mem obj.props prop) then (
    incr seq_counter;
    Hashtbl.replace obj.seq prop !seq_counter);
  Hashtbl.replace obj.props prop value

let remove (obj : t) (prop : string) =
  Hashtbl.remove obj.props prop;
  Hashtbl.remove obj.seq prop

(** Upstream: the property names in lexicographic order. Under the SERVPIPS
    semantics ([exec --servpips], [Config.servpips_semantics]; design E15,
    as the symbolic heap does): ES2020 OrdinaryOwnPropertyKeys order, i.e.
    array-index keys in ascending numeric order, then the other keys in
    creation order (a key deleted and added again comes last). *)
let properties (obj : t) : string list =
  if not !Config.servpips_semantics then
    Var.Set.elements
      (Hashtbl.fold
         (fun prop _ props -> Var.Set.add prop props)
         obj.props Var.Set.empty)
  else
    let names = Hashtbl.fold (fun prop _ ac -> prop :: ac) obj.props [] in
    let idx, named = List.partition SHeap.is_array_index names in
    let cmp_idx a b =
      let c = compare (String.length a) (String.length b) in
      if c <> 0 then c else String.compare a b
    in
    let seq k = Option.value ~default:max_int (Hashtbl.find_opt obj.seq k) in
    let cmp_named a b =
      let c = compare (seq a) (seq b) in
      if c <> 0 then c else String.compare a b
    in
    List.sort cmp_idx idx @ List.sort cmp_named named

(** GIL Typing Environment *)

open Names
open SVal
module L = Logging

type constructors_tbl_t = (string, Constructor.t) Hashtbl.t [@@deriving yojson]
type datatypes_tbl_t = (string, Datatype.t) Hashtbl.t [@@deriving yojson]
type tbl = (string, Type.t) Hashtbl.t [@@deriving yojson]

(* SERVPIPS (E19): [gen] is a stamp, fresh (globally unique) at creation and
   at every mutation, and kept by [copy] (a copied table iterates in the same
   order): two environments with the same stamp have the same bindings in the
   same iteration order (see [generation]). The JSON form is the table's, as
   before. *)
type t = { tbl : tbl; mutable gen : int }

let stamp_counter = ref 0

let fresh_stamp () =
  incr stamp_counter;
  !stamp_counter

let mk tbl = { tbl; gen = fresh_stamp () }
let to_yojson (x : t) = tbl_to_yojson x.tbl
let of_yojson j = Result.map mk (tbl_of_yojson j)
let as_hashtbl x = x.tbl
let generation (x : t) = x.gen
let servpips_set_generation (x : t) (g : int) = x.gen <- g
let touch (x : t) = x.gen <- fresh_stamp ()

(*************************************)
(** Typing Environment Functions **)

(*************************************)

(* Initialisation *)
let init () : t = mk (Hashtbl.create Config.medium_tbl_size)

(* Copy *)
let copy (x : t) : t = { tbl = Hashtbl.copy x.tbl; gen = x.gen }

(* Type of a variable *)
let get (x : t) (var : string) : Type.t option = Hashtbl.find_opt x.tbl var

(* Membership *)
let mem (x : t) (v : string) : bool = Hashtbl.mem x.tbl v

(* Empty *)
let empty (x : t) : bool = Hashtbl.length x.tbl == 0

(* Type of a variable *)
let get_exn (x : t) (var : string) : Type.t =
  match Hashtbl.find_opt x.tbl var with
  | Some t -> t
  | None ->
      raise (Failure ("Type_env.get_exn: variable " ^ var ^ " not found."))

(* Get all matchable elements *)
let matchables (x : t) : SS.t =
  Hashtbl.fold (fun var _ ac -> SS.add var ac) x.tbl SS.empty

(* Get all variables *)
let vars (x : t) : SS.t =
  Hashtbl.fold (fun var _ ac -> SS.add var ac) x.tbl SS.empty

(* Get all logical variables *)
let lvars (x : t) : SS.t =
  Hashtbl.fold
    (fun var _ ac -> if is_lvar_name var then SS.add var ac else ac)
    x.tbl SS.empty

(* Get all variables of specific type *)
let get_vars_of_type (x : t) (tt : Type.t) : string list =
  Hashtbl.fold
    (fun var t ac_vars -> if t = tt then var :: ac_vars else ac_vars)
    x.tbl []

(* Get all var-type pairs as a list *)
let get_var_type_pairs (x : t) : (string * Type.t) Seq.t = Hashtbl.to_seq x.tbl

(* Iteration *)
let iter (x : t) (f : string -> Type.t -> unit) : unit = Hashtbl.iter f x.tbl

let fold (x : t) (f : string -> Type.t -> 'a -> 'a) (init : 'a) : 'a =
  Hashtbl.fold f x.tbl init

let pp fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%s: %s)" v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

let pp_by_need vars fmt tenv =
  let pp_pair fmt (v, vt) = Fmt.pf fmt "(%s: %s)" v (Type.str vt) in
  let bindings = fold tenv (fun x t ac -> (x, t) :: ac) [] in
  let bindings = List.sort (fun (v, _) (w, _) -> Stdlib.compare v w) bindings in
  let bindings = List.filter (fun (v, _) -> SS.mem v vars) bindings in
  (Fmt.list ~sep:(Fmt.any "@\n") pp_pair) fmt bindings

(* Update with removal *)

let update (te : t) (x : string) (t : Type.t) : unit =
  match get te x with
  | None ->
      Hashtbl.replace te.tbl x t;
      touch te
  | Some t' when t' = t -> ()
  | Some t' ->
      Fmt.failwith
        "Type_env update: Conflict: %s has type %s but required extension is %s"
        x (Type.str t') (Type.str t)

let remove (te : t) (x : string) : unit =
  if Hashtbl.mem te.tbl x then (
    Hashtbl.remove te.tbl x;
    touch te)

(* Extend gamma with more_gamma *)
let extend (x : t) (y : t) : unit =
  iter y (fun v t ->
      match Hashtbl.find_opt x.tbl v with
      | None ->
          Hashtbl.replace x.tbl v t;
          touch x
      | Some t' ->
          if t <> t' then
            raise (Failure "Typing environment cannot be extended."))

(* Filter using function on variables *)
let filter (x : t) (f : string -> bool) : t =
  let new_gamma = init () in
  iter x (fun v v_type -> if f v then update new_gamma v v_type);
  new_gamma

(* Filter using function on variables *)
let filter_in_place (x : t) (f : string -> bool) : unit =
  let to_remove = fold x (fun v _ ac -> if f v then ac else v :: ac) [] in
  List.iter (remove x) to_remove

(* Filter for specific variables *)
let filter_vars (gamma : t) (vars : SS.t) : t =
  filter gamma (fun v -> SS.mem v vars)

(* Filter for specific variables *)
let filter_vars_in_place (gamma : t) (vars : SS.t) : unit =
  filter_in_place gamma (fun v -> SS.mem v vars)

(* Perform substitution, return new typing environment *)
let substitution (x : t) (subst : SESubst.t) (partial : bool) : t =
  let new_gamma = init () in
  iter x (fun var v_type ->
      let evar = Expr.from_var_name var in
      let new_var = SESubst.get subst evar in
      match new_var with
      | Some (LVar new_var) -> update new_gamma new_var v_type
      | Some _ -> if partial then update new_gamma var v_type
      | None ->
          if partial then update new_gamma var v_type
          else if Names.is_lvar_name var then (
            let new_lvar = LVar.alloc () in
            SESubst.put subst evar (LVar new_lvar);
            update new_gamma new_lvar v_type));
  new_gamma

let to_list_expr (x : t) : (Expr.t * Type.t) list =
  let le_type_pairs =
    Hashtbl.fold
      (fun x t (pairs : (Expr.t * Type.t) list) ->
        if Names.is_lvar_name x then (LVar x, t) :: pairs
        else (PVar x, t) :: pairs)
      x.tbl []
  in
  le_type_pairs

let to_list (x : t) : (Var.t * Type.t) list =
  let le_type_pairs =
    Hashtbl.fold
      (fun x t (pairs : (Var.t * Type.t) list) -> (x, t) :: pairs)
      x.tbl []
  in
  le_type_pairs

let reset (x : t) (reset : (Var.t * Type.t) list) =
  Hashtbl.clear x.tbl;
  List.iter (fun (y, t) -> Hashtbl.replace x.tbl y t) reset;
  touch x

let is_well_formed (_ : t) : bool = true

let filter_with_info relevant_info (x : t) =
  let pvars, lvars, locs = relevant_info in
  let relevant = List.fold_left SS.union SS.empty [ pvars; lvars; locs ] in
  filter x (fun x -> SS.mem x relevant)

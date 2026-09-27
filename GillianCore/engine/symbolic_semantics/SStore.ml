(** GIL Symbolic Store *)

open SVal
include Store.Make (SVal.M)

(** Updates --store-- to subst(store) *)
let substitution_in_place ?(subst_all = false) (subst : SESubst.t) (x : t) :
    unit =
  if not (SESubst.is_empty subst) then (
    let sp = !Config.servpips_semantics in
    let spec_for_spec u le =
      match (u, le) with
      | Expr.LVar x, Expr.LVar _ -> (not subst_all) && Names.is_spec_var_name x
      | _ -> false
    in
    (* Do not substitute spec vars for spec vars *)
    let store_subst =
      if sp && not (SESubst.fold subst (fun u le acc -> acc || spec_for_spec u le) false)
      then subst (* SERVPIPS (E19): nothing to change, no copy *)
      else (
        let store_subst = SESubst.copy subst in
        SESubst.filter_in_place store_subst (fun u le ->
            if spec_for_spec u le then Some u else Some le);
        store_subst)
    in
    if sp then (
      (* SERVPIPS (E19): the substitution (an endo visitor) returns the value
         itself when it mentions no substituted variable; such a value is not
         reduced again (it was reduced when it was computed), and the store
         table is only written for the values that change *)
      let changed =
        fold x
          (fun var value acc ->
            let substed =
              SESubst.subst_in_expr store_subst ~partial:true value
            in
            if substed == value then acc
            else (var, Reduction.reduce_lexpr substed) :: acc)
          []
      in
      List.iter (fun (var, v) -> put x var v) changed)
    else
      filter_map_inplace x (fun _ value ->
          let substed = SESubst.subst_in_expr store_subst ~partial:true value in
          Some (Reduction.reduce_lexpr substed)))

(** Returns the set containing all the vars occurring in --x-- *)
let vars (x : t) : SS.t =
  fold x (fun x le ac -> SS.union ac (SS.add x (Expr.vars le))) SS.empty

(** Returns the set containing all the alocs occurring in --x-- *)
let alocs (x : t) : SS.t =
  fold x (fun _ le ac -> SS.union ac (Expr.alocs le)) SS.empty

(** Returns the set containing all the alocs occurring in --x-- *)
let clocs (x : t) : SS.t =
  fold x (fun _ le ac -> SS.union ac (Expr.clocs le)) SS.empty

(** conversts a symbolic store to a list of assertions *)
let assertions (x : t) : Expr.t list =
  fold x
    (fun x le (assertions : Expr.t list) ->
      Expr.BinOp (PVar x, Equal, le) :: assertions)
    []

let is_well_formed (_ : t) : bool = true

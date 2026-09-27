open SVal
module L = Logging

(* SERVPIPS (E19): the formulae are kept in an [Ext_list] (order preserved,
   as upstream) together with an optional hash index of their multiplicities,
   so that [mem] (and hence [extend]) is O(1) instead of a linear scan with
   structural equality. The index is used only in SERVPIPS mode
   ([Config.servpips_semantics]); it is maintained by [extend] and rebuilt
   lazily after any other mutation (which just drops it). [mem] answers
   exactly as the list scan. *)
module H = Hashtbl.Make (struct
  type t = Expr.t

  let equal = Expr.equal
  let hash = Hashtbl.hash
end)

(* SERVPIPS (E19): [gen] is a stamp, fresh (globally unique) at creation and
   at every mutation, and kept by [copy]: two formula sets with the same stamp
   have the same formulae in the same order (see [generation]). *)
let stamp_counter = ref 0

let fresh_stamp () =
  incr stamp_counter;
  !stamp_counter

type t = {
  lst : Expr.t Ext_list.t;
  mutable idx : int H.t option;
  mutable gen : int;
}

let to_yojson (pfs : t) = Ext_list.to_yojson Expr.to_yojson pfs.lst

let of_yojson j =
  Result.map
    (fun lst -> { lst; idx = None; gen = fresh_stamp () })
    (Ext_list.of_yojson Expr.of_yojson j)

let mk lst = { lst; idx = None; gen = fresh_stamp () }
let generation (pfs : t) = pfs.gen
let servpips_set_generation (pfs : t) (g : int) = pfs.gen <- g

let invalidate (pfs : t) =
  pfs.idx <- None;
  pfs.gen <- fresh_stamp ()
let init () : t = mk (Ext_list.make ())

let equal (pfs1 : t) (pfs2 : t) : bool =
  Ext_list.for_all2 Expr.equal pfs1.lst pfs2.lst

let to_list (pfs : t) : Expr.t list = Ext_list.to_list pfs.lst
let of_list (l : Expr.t list) : t = mk (Ext_list.of_list l)

let to_set pfs =
  Ext_list.fold_left (fun acc el -> Expr.Set.add el acc) Expr.Set.empty pfs.lst

let index (pfs : t) : int H.t =
  match pfs.idx with
  | Some h -> h
  | None ->
      let h = H.create (max 16 (2 * Ext_list.length pfs.lst)) in
      Ext_list.iter
        (fun e ->
          H.replace h e (1 + Option.value (H.find_opt h e) ~default:0))
        pfs.lst;
      pfs.idx <- Some h;
      h

let mem_scan (pfs : t) (f : Expr.t) = Ext_list.mem ~equal:Expr.equal f pfs.lst

let mem (pfs : t) (f : Expr.t) =
  if !Config.servpips_semantics then H.mem (index pfs) f else mem_scan pfs f

let extend (pfs : t) (a : Expr.t) : unit =
  if not (mem pfs a) then (
    Ext_list.add a pfs.lst;
    pfs.gen <- fresh_stamp ();
    match pfs.idx with
    | Some h -> H.replace h a (1 + Option.value (H.find_opt h a) ~default:0)
    | None -> ())

let clear (pfs : t) : unit =
  Ext_list.clear pfs.lst;
  invalidate pfs

let length (pfs : t) = Ext_list.length pfs.lst

let copy (pfs : t) : t =
  { lst = Ext_list.copy pfs.lst; idx = Option.map H.copy pfs.idx; gen = pfs.gen }

let merge_into_left (pfs_l : t) (pfs_r : t) : unit =
  Ext_list.concat pfs_l.lst pfs_r.lst;
  invalidate pfs_l;
  invalidate pfs_r

let set (pfs : t) (reset : Expr.t list) : unit =
  clear pfs;
  merge_into_left pfs (of_list reset)

let substitution (subst : SESubst.t) (pfs : t) : unit =
  Ext_list.map_inplace (SESubst.subst_in_expr ~partial:true subst) pfs.lst;
  invalidate pfs

let subst_expr_for_expr (to_subst : Expr.t) (subst_with : Expr.t) (pfs : t) :
    unit =
  Ext_list.map_inplace
    (Expr.subst_expr_for_expr ~to_subst ~subst_with)
    pfs.lst;
  invalidate pfs

let lvars (pfs : t) : SS.t =
  Ext_list.fold_left (fun ac a -> SS.union ac (Expr.lvars a)) SS.empty pfs.lst

let alocs (pfs : t) : SS.t =
  Ext_list.fold_left (fun ac a -> SS.union ac (Expr.alocs a)) SS.empty pfs.lst

let clocs (pfs : t) : SS.t =
  Ext_list.fold_left (fun ac a -> SS.union ac (Expr.clocs a)) SS.empty pfs.lst

let pp fmt (pfs : t) = Fmt.vbox (Ext_list.pp ~sep:Fmt.cut Expr.pp) fmt pfs.lst

let sort (p_formulae : t) : unit =
  let pfl = to_list p_formulae in
  let var_eqs, llen_eqs, others =
    List.fold_left
      (fun (var_eqs, llen_eqs, others) (pf : Expr.t) ->
        match pf with
        | BinOp (LVar _, Equal, _) | BinOp (_, Equal, LVar _) ->
            (pf :: var_eqs, llen_eqs, others)
        | BinOp (UnOp (LstLen, _), Equal, _) | BinOp (_, Equal, UnOp (LstLen, _))
          -> (var_eqs, pf :: llen_eqs, others)
        | _ -> (var_eqs, llen_eqs, pf :: others))
      ([], [], []) pfl
  in
  let var_eqs, llen_eqs, others =
    (List.rev var_eqs, List.rev llen_eqs, List.rev others)
  in
  set p_formulae (var_eqs @ llen_eqs @ others)

let iter f (pfs : t) = Ext_list.iter f pfs.lst
let fold_left f acc (pfs : t) = Ext_list.fold_left f acc pfs.lst

let map_inplace f (pfs : t) =
  Ext_list.map_inplace f pfs.lst;
  invalidate pfs

let remove_duplicates (pfs : t) =
  Ext_list.remove_duplicates pfs.lst;
  invalidate pfs

let filter_map_stop f (pfs : t) =
  let r = Ext_list.filter_map_stop f pfs.lst in
  invalidate pfs;
  r

let filter_stop_cond ~keep ~cond (pfs : t) =
  let r = Ext_list.filter_stop_cond ~keep ~cond pfs.lst in
  invalidate pfs;
  r

let filter f (pfs : t) =
  Ext_list.filter f pfs.lst;
  invalidate pfs

let filter_map f (pfs : t) =
  Ext_list.filter_map f pfs.lst;
  invalidate pfs

let exists f (pfs : t) = Ext_list.exists f pfs.lst
let get_nth n (pfs : t) = Ext_list.nth n pfs.lst

let clean_up pfs =
  filter
    (fun (pf : Expr.t) ->
      match pf with
      | Expr.BinOp (Lit (Int x), BinOp.ILessThanEqual, UnOp (LstLen, _))
        when x = Z.zero -> false
      | _ -> true)
    pfs

let rec get_relevant_info (_ : SS.t) (lvars : SS.t) (locs : SS.t) (pfs : t) :
    SS.t * SS.t * SS.t =
  let relevant = SS.union lvars locs in
  let new_pvars, new_lvars, new_locs =
    fold_left
      (fun (new_pvars, new_lvars, new_locs) pf ->
        let pf_pvars = Expr.pvars pf in
        let pf_lvars = Expr.lvars pf in
        let pf_locs = Expr.locs pf in
        let pf_relevant = SS.union pf_pvars (SS.union pf_lvars pf_locs) in
        if SS.inter relevant pf_relevant = SS.empty then
          (new_pvars, new_lvars, new_locs)
        else
          ( SS.union new_pvars pf_pvars,
            SS.union new_lvars pf_lvars,
            SS.union new_locs pf_locs ))
      (SS.empty, SS.empty, SS.empty)
      pfs
  in
  if new_lvars = lvars && new_locs = locs then (new_pvars, new_lvars, new_locs)
  else get_relevant_info new_pvars new_lvars new_locs pfs

let filter_with_info relevant_info (pfs : t) : t =
  let pvars, lvars, locs = relevant_info in

  let _, lvars, locs = get_relevant_info pvars lvars locs pfs in

  let relevant = List.fold_left SS.union SS.empty [ lvars; locs ] in
  let filtered_pfs = copy pfs in
  let () =
    filter
      (fun pf ->
        let pf_info = SS.union (Expr.lvars pf) (Expr.locs pf) in
        let overlap = SS.inter relevant pf_info in
        not @@ SS.is_empty overlap)
      filtered_pfs
  in
  filtered_pfs

let pp_by_need relevant_info fmt pfs =
  let filtered_pfs = filter_with_info relevant_info pfs in
  pp fmt filtered_pfs

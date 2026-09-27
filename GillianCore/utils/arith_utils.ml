(** Utility functions for floating point arithmetic *)

(** Checks if a float is an integer

    Note that [-0] is {b not} considered an integer *)
let is_int (f : float) : bool =
  let f' = float_of_int (int_of_float f) in
  f = f' && copysign 1.0 f = copysign 1.0 f'

(** Checks if a flot is not NaN or infinite *)
let is_normal (f : float) =
  let fc = Float.classify_float f in
  not (fc = FP_infinite || fc = FP_nan)

(** Rounds a float towards 0

    Returns 0 if NaN, and unchanged if infinite*)
let to_int n =
  match classify_float n with
  | FP_nan -> 0.
  | FP_infinite -> n
  | FP_zero -> n
  | FP_normal | FP_subnormal ->
      (if n < 0. then -1. else 1.) *. floor (abs_float n)

(** Same as {!to_int}, but overflows as if it's a signed 32-bit int *)
let to_int32 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i32 = 2. ** 32. in
      let i31 = 2. ** 31. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int32bit =
        let smod = mod_float posint i32 in
        if smod < 0. then smod +. i32 else smod
      in
      if int32bit >= i31 then int32bit -. i32 else int32bit
  | _ -> 0.

(** Same as {!to_int32}, but for unsigned 32-bit ints *)
let to_uint32 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i32 = 2. ** 32. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int32bit =
        let smod = mod_float posint i32 in
        if smod < 0. then smod +. i32 else smod
      in
      int32bit
  | _ -> 0.

(** Same as {!to_uint32}, but for unsigned 16-bit ints *)
let to_uint16 n =
  match classify_float n with
  | FP_normal | FP_subnormal ->
      let i16 = 2. ** 16. in
      let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
      let int16bit =
        let smod = mod_float posint i16 in
        if smod < 0. then smod +. i16 else smod
      in
      int16bit
  | _ -> 0.

let int64_bitwise_not = Z.lognot
let int64_bitwise_and = Z.logand
let int64_bitwise_or = Z.logor
let int64_bitwise_xor = Z.logxor
let int64_left_shift x y = Z.shift_left x (Z.to_int y)

let int64_right_shift x y =
  let l = Int64.of_float x in
  let r = int_of_float y in
  Int64.to_float (Int64.shift_right l r)

let uint64_right_shift x y =
  let l = Int64.of_float x in
  let r = int_of_float y in
  Int64.to_float (Int64.shift_right_logical l r)

let int32_bitwise_not x = Int32.to_float (Int32.lognot (Int32.of_float x))

let int32_bitwise_and x y =
  Int32.to_float (Int32.logand (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_or x y =
  Int32.to_float (Int32.logor (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_xor x y =
  Int32.to_float (Int32.logxor (Int32.of_float x) (Int32.of_float y))

let int32_left_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_left l r)

let int32_right_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_right l r)

let uint32_right_shift x y = Z.shift_right x (Z.to_int y)

let uint32_right_shift_f x y =
  let i31 = 2. ** 31. in
  let i32 = 2. ** 32. in
  let signedx = if x >= i31 then x -. i32 else x in
  let left = Int32.of_float signedx in
  let right = int_of_float y mod 32 in
  let r = Int32.to_float (Int32.shift_right_logical left right) in
  if r < 0. then r +. i32 else r

let uint64_int_right_shift x y = Z.shift_right x (Z.to_int y)

(** Stringifies a float, adapting based on its size, or whether it's an integer

    Assumes the float is normal and positive *)
let string_of_pos_float num =
  (* Is the number an integer? *)
  let inum = int_of_float num in
  if is_int num then string_of_int inum (* It is not an integer *)
  else if num > 1e+9 && num < 1e+21 then Printf.sprintf "%.0f" num
  else if 1e-5 <= num && num < 1e-4 then
    let s = Float.to_string (num *. 10.) in
    let len = String.length s in
    "0.0" ^ String.sub s 2 (len - 2)
  else if 1e-6 <= num && num < 1e-5 then
    let s = Float.to_string (num *. 100.) in
    let len = String.length s in
    "0.00" ^ String.sub s 2 (len - 2)
  else
    let re = Str.regexp "e\\([-+]\\)0" in
    (* e+0 -> e+ *)
    Str.replace_first re "e\\1" (Float.to_string num)

(** Stringifies a float, considering negative and abnormal cases *)
let rec float_to_string_inner n =
  if Float.is_nan n then "NaN"
  else if n = 0.0 || n = -0.0 then "0"
  else if n < 0.0 then "-" ^ float_to_string_inner (-.n)
  else if n = Float.infinity then "Infinity"
  else string_of_pos_float n

(* ------------------------------------------------------------------ *)
(* SERVPIPS: ECMAScript conversions (E16 StringToNumber, E8            *)
(* Number::toString)                                                    *)
(* ------------------------------------------------------------------ *)

(** StrWhiteSpaceChar of ES2023 (WhiteSpace and LineTerminator), as code
    points; Zs as in Node 18/20 (Unicode 15). *)
let js_is_whitespace_cp (c : int) : bool =
  (c >= 9 && c <= 13)
  || c = 32 || c = 0xA0 || c = 0x1680
  || (c >= 0x2000 && c <= 0x200A)
  || c = 0x2028 || c = 0x2029 || c = 0x202F || c = 0x205F || c = 0x3000
  || c = 0xFEFF

(* decode the UTF-8 code point starting at byte [i]: (code point, length) *)
let utf8_decode_at (s : string) (i : int) : (int * int) option =
  let n = String.length s in
  let b k = Char.code s.[k] in
  let cont k = k < n && b k land 0xC0 = 0x80 in
  if i >= n then None
  else
    let c = b i in
    if c < 0x80 then Some (c, 1)
    else if c land 0xE0 = 0xC0 && cont (i + 1) then
      Some (((c land 0x1F) lsl 6) lor (b (i + 1) land 0x3F), 2)
    else if c land 0xF0 = 0xE0 && cont (i + 1) && cont (i + 2) then
      Some
        ( ((c land 0x0F) lsl 12)
          lor ((b (i + 1) land 0x3F) lsl 6)
          lor (b (i + 2) land 0x3F),
          3 )
    else if c land 0xF8 = 0xF0 && cont (i + 1) && cont (i + 2) && cont (i + 3)
    then
      Some
        ( ((c land 0x07) lsl 18)
          lor ((b (i + 1) land 0x3F) lsl 12)
          lor ((b (i + 2) land 0x3F) lsl 6)
          lor (b (i + 3) land 0x3F),
          4 )
    else None

(** Removes leading and trailing StrWhiteSpaceChar from a GIL string (UTF-8;
    an invalid byte is not whitespace). *)
let js_trim (s : string) : string =
  let n = String.length s in
  let rec lead i =
    match utf8_decode_at s i with
    | Some (cp, l) when js_is_whitespace_cp cp -> lead (i + l)
    | _ -> i
  in
  let start = lead 0 in
  let rec trail j =
    if j <= start then start
    else
      (* start of the code point that ends at j *)
      let k = ref (j - 1) in
      while !k > start && j - !k < 4 && Char.code s.[!k] land 0xC0 = 0x80 do
        decr k
      done;
      match utf8_decode_at s !k with
      | Some (cp, l) when !k + l = j && js_is_whitespace_cp cp -> trail !k
      | _ -> j
  in
  let stop = trail n in
  String.sub s start (stop - start)

(* An arbitrary-size non-negative integer to the nearest double (ties to
   even), through OCaml's hexadecimal float parser. *)
let z_to_float_nearest (z : Z.t) : float =
  if Z.numbits z <= 53 then Z.to_float z
  else float_of_string ("0x" ^ Z.format "%x" z)

(** ES2023 StringToNumber (7.1.4.1.1) of a GIL string: whitespace trimmed,
    empty -> 0, [Infinity] with optional sign, decimal literals with optional
    sign, fraction and exponent (no numeric separators), unsigned
    [0x]/[0o]/[0b] integers; anything else is NaN. *)
let js_string_to_number (s : string) : float =
  let t = js_trim s in
  let len = String.length t in
  let is_digit c = c >= '0' && c <= '9' in
  let all_from i p =
    i < len
    &&
    let ok = ref true in
    for j = i to len - 1 do
      if not (p t.[j]) then ok := false
    done;
    !ok
  in
  if len = 0 then 0.
  else
    match t with
    | "Infinity" | "+Infinity" -> Float.infinity
    | "-Infinity" -> Float.neg_infinity
    | _ ->
        let nondecimal =
          if len >= 3 && t.[0] = '0' then
            let digits = String.sub t 2 (len - 2) in
            match t.[1] with
            | 'x' | 'X'
              when all_from 2 (fun c ->
                       is_digit c
                       || (c >= 'a' && c <= 'f')
                       || (c >= 'A' && c <= 'F')) ->
                Some (z_to_float_nearest (Z.of_string_base 16 digits))
            | 'o' | 'O' when all_from 2 (fun c -> c >= '0' && c <= '7') ->
                Some (z_to_float_nearest (Z.of_string_base 8 digits))
            | 'b' | 'B' when all_from 2 (fun c -> c = '0' || c = '1') ->
                Some (z_to_float_nearest (Z.of_string_base 2 digits))
            | _ -> None
          else None
        in
        match nondecimal with
        | Some f -> f
        | None ->
            (* [+-]? (D+ (. D* )? | . D+) ([eE] [+-]? D+)? *)
            let i = ref 0 in
            let neg = len > 0 && t.[0] = '-' in
            if len > 0 && (t.[0] = '+' || t.[0] = '-') then incr i;
            let digits_to j =
              let k = ref j in
              while !k < len && is_digit t.[!k] do
                incr k
              done;
              !k
            in
            let m0 = !i in
            let j1 = digits_to m0 in
            let int_digits = j1 - m0 in
            let j2, frac_digits =
              if j1 < len && t.[j1] = '.' then
                let j = digits_to (j1 + 1) in
                (j, j - (j1 + 1))
              else (j1, 0)
            in
            let ok_mantissa = int_digits > 0 || frac_digits > 0 in
            let ok_exp, stop =
              if j2 < len && (t.[j2] = 'e' || t.[j2] = 'E') then
                let k = ref (j2 + 1) in
                if !k < len && (t.[!k] = '+' || t.[!k] = '-') then incr k;
                let e_end = digits_to !k in
                (e_end > !k, e_end)
              else (true, j2)
            in
            if ok_mantissa && ok_exp && stop = len then
              (* a validated decimal literal: OCaml's parser (strtod) rounds
                 it correctly *)
              let body = String.sub t m0 (len - m0) in
              let body = if body.[0] = '.' then "0" ^ body else body in
              let v = float_of_string body in
              if neg then -.v else v
            else Float.nan

(* The shortest decimal digit string d (k digits, no trailing zeros) and n
   such that d * 10^(n-k) rounds to [x] (x > 0, finite); among the
   k-digit candidates the closest to x, then the even one (ES 6.1.6.1.20). *)
let js_shortest_digits (x : float) : string * int =
  let parse_e str =
    (* "d.ddde[+-]XX" *)
    let e_idx = String.index str 'e' in
    let mant = String.sub str 0 e_idx in
    let exp = int_of_string (String.sub str (e_idx + 1) (String.length str - e_idx - 1)) in
    let digits = String.concat "" (String.split_on_char '.' mant) in
    (digits, exp)
  in
  let value digits exp10 = float_of_string (Printf.sprintf "%se%d" digits exp10) in
  let qx = Q.of_float x in
  let q_of digits exp10 =
    let m = Q.of_bigint (Z.of_string digits) in
    if exp10 >= 0 then Q.mul m (Q.of_bigint (Z.pow (Z.of_int 10) exp10))
    else Q.div m (Q.of_bigint (Z.pow (Z.of_int 10) (-exp10)))
  in
  let strip digits exp =
    (* remove trailing zeros; exp is the exponent of the first digit *)
    let k = ref (String.length digits) in
    while !k > 1 && digits.[!k - 1] = '0' do
      decr k
    done;
    (String.sub digits 0 !k, exp)
  in
  let rec go p =
    let str = Printf.sprintf "%.*e" p x in
    let digits, exp = parse_e str in
    (* digits has p+1 digits; value = digits * 10^(exp - p) *)
    let k = p + 1 in
    let cands =
      let m = Z.of_string digits in
      let mk m' =
        let s' = Z.to_string m' in
        if String.length s' = k then Some (s', exp)
        else if String.length s' = k + 1 then
          (* carry: 10^k -> 1 followed by zeros, exponent + 1 *)
          Some (String.sub s' 0 k, exp + 1)
        else if String.length s' = k - 1 && k > 1 then
          (* borrow: 10^(k-1) - 1 = k-1 nines, exponent - 1 *)
          Some (s' ^ "9", exp - 1)
        else None
      in
      List.filter_map mk [ m; Z.pred m; Z.succ m ]
    in
    let round_trips =
      List.filter
        (fun (d, e) -> Float.equal (value d (e - (String.length d - 1))) x)
        cands
    in
    match round_trips with
    | [] -> if p >= 16 then parse_e (Printf.sprintf "%.16e" x) |> fun (d, e) -> strip d e else go (p + 1)
    | _ ->
        let dist (d, e) =
          Q.abs (Q.sub (q_of d (e - (String.length d - 1))) qx)
        in
        let best =
          List.fold_left
            (fun acc c ->
              match acc with
              | None -> Some c
              | Some b ->
                  let cmp = Q.compare (dist c) (dist b) in
                  if cmp < 0 then Some c
                  else if cmp = 0 then
                    let last (d, _) = Char.code d.[String.length d - 1] land 1 in
                    if last c = 0 && last b = 1 then Some c else acc
                  else acc)
            None round_trips
        in
        let d, e = Option.get best in
        strip d e
  in
  let d, e = go 0 in
  (d, e + 1)

(** ES2023 Number::toString(x) (radix 10). *)
let rec js_number_to_string (x : float) : string =
  if Float.is_nan x then "NaN"
  else if x = 0. then "0"
  else if x < 0. then "-" ^ js_number_to_string (-.x)
  else if x = Float.infinity then "Infinity"
  else
    let digits, n = js_shortest_digits x in
    let k = String.length digits in
    if k <= n && n <= 21 then digits ^ String.make (n - k) '0'
    else if 0 < n && n <= 21 then
      String.sub digits 0 n ^ "." ^ String.sub digits n (k - n)
    else if -6 < n && n <= 0 then "0." ^ String.make (-n) '0' ^ digits
    else
      let e = n - 1 in
      let es = (if e >= 0 then "+" else "-") ^ string_of_int (abs e) in
      if k = 1 then digits ^ "e" ^ es
      else String.sub digits 0 1 ^ "." ^ String.sub digits 1 (k - 1) ^ "e" ^ es

(** Math.sign *)
let js_sign (x : float) : float =
  if Float.is_nan x || x = 0. then x else if x > 0. then 1. else -1.

let string_split (s : string) (len : int) : string option * string option =
  assert (len > 0);
  try
    let char = String.sub s 0 len in
    let str = String.sub s len (String.length s - len) in
    (Some char, Some str)
  with Invalid_argument _ -> (None, None)

let string_head_and_tail s = string_split s 1

let surprise_substitute_string =
  "ItIsExtremelyUnlikelyForThisStringToExistInTheProgram"

type state =
  | TheUsual
  | InString
  | InAsmOrAsrt of int * string
  | InAsmOrAsrtAndInString of int * string

exception Unparseable of string

let rec stringify_assume_and_assert_aux
    (s_in : string)
    (s_out : string)
    (s : state) : string =
  let f = stringify_assume_and_assert_aux in
  match s_in with
  | "" -> (
      match s with
      | TheUsual -> s_out
      | InString ->
          raise (Unparseable "Reached EOF: non-terminated string literal.")
      | InAsmOrAsrt _ ->
          raise (Unparseable "Reached EOF: assume or assert not terminated.")
      | InAsmOrAsrtAndInString _ ->
          raise
            (Unparseable
               "Reached EOF: a string literal inside assume or assert not \
                terminated."))
  | _ -> (
      let next_char, rest_of_string = string_head_and_tail s_in in
      assert (next_char <> None);
      assert (rest_of_string <> None);
      let next_char, rest_of_string =
        (Option.get next_char, Option.get rest_of_string)
      in
      match s with
      | TheUsual -> (
          match next_char with
          | "\"" -> f rest_of_string (s_out ^ "\"") InString
          | "A" -> (
              let next_five_chars, new_rest_of_string =
                string_split rest_of_string 5
              in
              match (next_five_chars, new_rest_of_string) with
              | Some next_five_chars, Some new_rest_of_string
                when next_five_chars = "ssume" || next_five_chars = "ssert" ->
                  f new_rest_of_string
                    (s_out ^ "A" ^ next_five_chars)
                    (InAsmOrAsrt (0, ""))
              | _, _ -> f rest_of_string (s_out ^ "A") TheUsual)
          | _ -> f rest_of_string (s_out ^ next_char) TheUsual)
      | InString -> (
          match next_char with
          | "\"" -> f rest_of_string (s_out ^ "\"") TheUsual
          | "\\" -> (
              let next_char, rest_of_string =
                string_head_and_tail rest_of_string
              in
              match (next_char, rest_of_string) with
              | None, None ->
                  raise
                    (Unparseable "Reached EOF: non-terminated string literal.")
              | Some nc, Some rest_of_string ->
                  f rest_of_string (s_out ^ "\\" ^ nc) InString
              | _ ->
                  failwith
                    "Unhandled case stringify_assume_and_assert_aux.InString")
          | _ -> f rest_of_string (s_out ^ next_char) InString)
      | InAsmOrAsrt (depth, str) -> (
          assert (depth >= 0);
          match next_char with
          | "(" -> (
              match depth with
              | 0 -> f rest_of_string (s_out ^ "(") (InAsmOrAsrt (1, str))
              | _ ->
                  f rest_of_string s_out
                    (InAsmOrAsrt (depth + 1, str ^ next_char)))
          | ")" -> (
              match depth with
              | 1 ->
                  let str =
                    Str.global_replace (Str.regexp "\\\\\"")
                      surprise_substitute_string str
                  in
                  let str = Str.global_replace (Str.regexp "\"") "\\\"" str in
                  let str =
                    Str.global_replace
                      (Str.regexp surprise_substitute_string)
                      "\\\\\\\\\\\"" str
                  in
                  f rest_of_string (s_out ^ "\"" ^ str ^ "\")") TheUsual
              | _ ->
                  f rest_of_string s_out
                    (InAsmOrAsrt (depth - 1, str ^ next_char)))
          | (" " | "\t") when depth = 0 ->
              f rest_of_string (s_out ^ next_char) (InAsmOrAsrt (depth, str))
          | _ -> (
              match depth with
              | 0 ->
                  raise
                    (Unparseable
                       ("Parsing error: assume or assert not followed by an \
                         open parenthesis but by " ^ next_char ^ "."))
              | _ -> (
                  match next_char with
                  | "\"" ->
                      f rest_of_string s_out
                        (InAsmOrAsrtAndInString (depth, str ^ next_char))
                  | _ ->
                      f rest_of_string s_out
                        (InAsmOrAsrt (depth, str ^ next_char)))))
      | InAsmOrAsrtAndInString (depth, str) -> (
          match next_char with
          | "\"" -> f rest_of_string s_out (InAsmOrAsrt (depth, str ^ "\""))
          | "\\" -> (
              let next_char, rest_of_string =
                string_head_and_tail rest_of_string
              in
              match (next_char, rest_of_string) with
              | None, None ->
                  raise
                    (Unparseable
                       "Reached EOF: a string literal inside assume or assert \
                        not terminated.")
              | Some nc, Some rest_of_string ->
                  f rest_of_string s_out
                    (InAsmOrAsrtAndInString (depth, str ^ "\\" ^ nc))
              | _ ->
                  failwith
                    "Unhandled case \
                     stringify_assume_and_assert_aux.InAsmOrAsrtAndInString")
          | _ ->
              f rest_of_string s_out
                (InAsmOrAsrtAndInString (depth, str ^ next_char))))

let stringify_assume_and_assert (s : string) =
  stringify_assume_and_assert_aux s "" TheUsual

(* SERVPIPS (--servpips, design R4): a token-aware version of the Cosette
   pre-parser.

   The upstream pre-parser above rewrites the text [Assert(e)] / [Assume(e)]
   to [Assert("e")] wherever the characters appear outside a double-quoted
   string: it rewrites method calls ([o.Assert(1)] becomes [o.Assert("1")], a
   silent semantic change) and raises on identifiers such as [AssertionError]
   / [AssumeRole] and on a lone double quote in a comment, a single-quoted
   string or a regular expression. This version lexes the source (comments,
   the three kinds of string literals with template substitutions, regular
   expression literals) and rewrites only a call whose callee is the bare
   identifier [Assert] / [Assume]: not a property ([.Assert]), not a longer
   identifier, not after [function] / [new]. The argument text up to the
   matching parenthesis becomes a string literal (backslashes and double
   quotes escaped, so the compiler reads back the exact text). It never
   raises: on anything it cannot lex (an unterminated literal or comment,
   unbalanced parentheses) it returns the source unchanged and the JS parser
   / compiler reports the error (a Cosette assertion that is not stringified
   is a compile error, never a different program). *)

exception Servpips_bail

(* the kind of the last significant token, for the regular-expression /
   division ambiguity of "/" and for the callee test *)
type sp_prev =
  | Sp_start  (** start of input: "/" starts a regular expression *)
  | Sp_operand  (** identifier, literal, ")", "]", "}": "/" is a division *)
  | Sp_operator  (** punctuator or keyword before an expression *)
  | Sp_dot  (** "." *)
  | Sp_decl  (** [function] / [new]: the next identifier is not a callee *)

let sp_regex_keywords =
  [
    "return"; "typeof"; "instanceof"; "in"; "of"; "new"; "delete"; "void";
    "throw"; "case"; "do"; "else"; "yield"; "await";
  ]

let servpips_stringify_assume_and_assert (src : string) : string =
  let n = String.length src in
  let is_id_start c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || c = '_' || c = '$' || Char.code c >= 128
  in
  let is_id_char c = is_id_start c || (c >= '0' && c <= '9') in
  let is_digit c = c >= '0' && c <= '9' in
  let peek j = if j < n then Some src.[j] else None in
  (* index after the string literal opened by the quote at [i] *)
  let rec skip_string i q =
    let rec go j =
      if j >= n then raise Servpips_bail
      else
        let c = src.[j] in
        if c = '\\' then go (j + 2)
        else if c = q then j + 1
        else if c = '\n' || c = '\r' then raise Servpips_bail
        else go (j + 1)
    in
    go (i + 1)
  (* index after the template literal opened at [i] *)
  and skip_template i =
    let rec go j =
      if j >= n then raise Servpips_bail
      else
        match src.[j] with
        | '\\' -> go (j + 2)
        | '`' -> j + 1
        | '$' when peek (j + 1) = Some '{' ->
            let close = skip_balanced (j + 2) '}' in
            go (close + 1)
        | _ -> go (j + 1)
    in
    go (i + 1)
  (* index after the regular expression literal opened at [i] *)
  and skip_regex i =
    let rec go j in_class =
      if j >= n then raise Servpips_bail
      else
        match src.[j] with
        | '\\' -> go (j + 2) in_class
        | '\n' | '\r' -> raise Servpips_bail
        | '[' -> go (j + 1) true
        | ']' -> go (j + 1) false
        | '/' when not in_class ->
            let rec flags k =
              if k < n && is_id_char src.[k] then flags (k + 1) else k
            in
            flags (j + 1)
        | _ -> go (j + 1) in_class
    in
    go (i + 1) false
  (* index after the comment starting at [i] ("//" or "/*"), or [i] *)
  and skip_comment i =
    match (peek i, peek (i + 1)) with
    | Some '/', Some '/' ->
        let rec go j =
          if j >= n || src.[j] = '\n' || src.[j] = '\r' then j else go (j + 1)
        in
        go (i + 2)
    | Some '/', Some '*' ->
        let rec go j =
          if j + 1 >= n then raise Servpips_bail
          else if src.[j] = '*' && src.[j + 1] = '/' then j + 2
          else go (j + 1)
        in
        go (i + 2)
    | _ -> i
  (* the index of the unmatched [close] (")" or "}") after [i] *)
  and skip_balanced i close =
    let rec go j prev stack =
      if j >= n then raise Servpips_bail
      else
        let c = src.[j] in
        if c = '/' && (peek (j + 1) = Some '/' || peek (j + 1) = Some '*')
        then go (skip_comment j) prev stack
        else
          match c with
          | ' ' | '\t' | '\n' | '\r' -> go (j + 1) prev stack
          | '"' | '\'' -> go (skip_string j c) Sp_operand stack
          | '`' -> go (skip_template j) Sp_operand stack
          | '/' when prev <> Sp_operand -> go (skip_regex j) Sp_operand stack
          | '(' | '[' | '{' ->
              let m =
                match c with
                | '(' -> ')'
                | '[' -> ']'
                | _ -> '}'
              in
              go (j + 1) Sp_operator (m :: stack)
          | ')' | ']' | '}' -> (
              match stack with
              | m :: rest when m = c -> go (j + 1) Sp_operand rest
              | [] when c = close -> j
              | _ -> raise Servpips_bail)
          | c when is_id_start c ->
              let k = ref (j + 1) in
              while !k < n && is_id_char src.[!k] do
                incr k
              done;
              let w = String.sub src j (!k - j) in
              let prev =
                if List.mem w sp_regex_keywords then Sp_operator else Sp_operand
              in
              go !k prev stack
          | c when is_digit c ->
              let k = ref (j + 1) in
              while !k < n && (is_id_char src.[!k] || src.[!k] = '.') do
                incr k
              done;
              go !k Sp_operand stack
          | _ -> go (j + 1) Sp_operator stack
    in
    go i Sp_start []
  in
  let buf = Buffer.create (n + 64) in
  let add_sub i j = Buffer.add_string buf (String.sub src i (j - i)) in
  let add_escaped s =
    String.iter
      (fun c ->
        match c with
        | '\\' -> Buffer.add_string buf "\\\\"
        | '"' -> Buffer.add_string buf "\\\""
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | c -> Buffer.add_char buf c)
      s
  in
  let rec main i prev =
    if i < n then
      let c = src.[i] in
      if c = '/' && (peek (i + 1) = Some '/' || peek (i + 1) = Some '*') then (
        let j = skip_comment i in
        add_sub i j;
        main j prev)
      else
        match c with
        | ' ' | '\t' | '\n' | '\r' ->
            Buffer.add_char buf c;
            main (i + 1) prev
        | '"' | '\'' ->
            let j = skip_string i c in
            add_sub i j;
            main j Sp_operand
        | '`' ->
            let j = skip_template i in
            add_sub i j;
            main j Sp_operand
        | '/' when prev <> Sp_operand ->
            let j = skip_regex i in
            add_sub i j;
            main j Sp_operand
        | '.' when not (match peek (i + 1) with Some d -> is_digit d | None -> false) ->
            Buffer.add_char buf c;
            main (i + 1) Sp_dot
        | c when is_digit c || c = '.' ->
            let k = ref (i + 1) in
            while !k < n && (is_id_char src.[!k] || src.[!k] = '.') do
              incr k
            done;
            add_sub i !k;
            main !k Sp_operand
        | c when is_id_start c ->
            let k = ref (i + 1) in
            while !k < n && is_id_char src.[!k] do
              incr k
            done;
            let j = !k in
            let w = String.sub src i (j - i) in
            let callee_position =
              match prev with
              | Sp_dot | Sp_decl -> false
              | _ -> true
            in
            if (w = "Assert" || w = "Assume") && callee_position then (
              (* whitespace and comments between the name and "(" *)
              let rec gap k =
                if k >= n then k
                else
                  match src.[k] with
                  | ' ' | '\t' | '\n' | '\r' -> gap (k + 1)
                  | '/' when peek (k + 1) = Some '/' || peek (k + 1) = Some '*'
                    -> gap (skip_comment k)
                  | _ -> k
              in
              let k = gap j in
              if k < n && src.[k] = '(' then (
                let close = skip_balanced (k + 1) ')' in
                Buffer.add_string buf w;
                add_sub j k;
                Buffer.add_string buf "(\"";
                add_escaped (String.sub src (k + 1) (close - k - 1));
                Buffer.add_string buf "\")";
                main (close + 1) Sp_operand)
              else (
                add_sub i j;
                main j Sp_operand))
            else (
              add_sub i j;
              let prev =
                if w = "function" || w = "new" then Sp_decl
                else if List.mem w sp_regex_keywords then Sp_operator
                else Sp_operand
              in
              main j prev)
        | ')' | ']' | '}' ->
            Buffer.add_char buf c;
            main (i + 1) Sp_operand
        | _ ->
            Buffer.add_char buf c;
            main (i + 1) Sp_operator
  in
  try
    main 0 Sp_start;
    Buffer.contents buf
  with Servpips_bail | Invalid_argument _ -> src

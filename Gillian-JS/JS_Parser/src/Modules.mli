open Flow_parser.Flow_ast
open Flow_parser

type loc = Loc.t
type file = Flow_parser.File_key.t
type error = Flow_parser.Parse_error.t

type parse_f =
  string -> file option -> (loc, loc) Program.t * (loc * error) list

type transform_f = (loc, loc) Program.t -> GJS_syntax.exp

val parse_commonjs :
  parse_f -> transform_f -> string -> string -> GJS_syntax.exp

(** When [Some dir], the CommonJS preamble ([preamble.js]) is read from [dir]
    instead of the directory named by [GILLIAN_JS_RUNTIME_PATH]. *)
val preamble_dir_override : string option ref

open Ast

type entry =
  | VariableEntry of expr
  | FunctionEntry of function_binding * dynamic_env
[@@deriving show]
and dynamic_env = (string * entry) list [@@deriving show]

let string_of_entry = show_entry

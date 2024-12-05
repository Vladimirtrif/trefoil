type pattern =
  | WildcardPattern
  | VarPattern of string
  | IntPattern of int
  | BoolPattern of bool
  | NilPattern
  | SymbolPattern of string
  | ConsPattern of pattern * pattern
  | StructPattern of string * pattern list
[@@deriving show]
let string_of_pattern = show_pattern

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  | Car of expr
  | Cdr of expr
  | Call of string * expr list
  | Cond of (expr * expr) list
  | Symbol of string
[@@deriving show]

and function_binding = { name: string; param_names: string list; body: expr }
[@@deriving show]

and binding =
   | VarBinding of string * expr
   | TopLevelExpr of expr
   | FunctionBinding of function_binding
   | TestBinding of expr
[@@deriving show]

and entry =
  | VariableEntry of expr
  | FunctionEntry of function_binding * dynamic_env
[@@deriving show]
and dynamic_env = (string * entry) list [@@deriving show]

let string_of_expr = show_expr
let string_of_binding = show_binding
let string_of_entry = show_entry
let string_of_dynenv_entry (x, e) = x ^ " -> " ^ string_of_entry e
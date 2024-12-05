type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  | Car of expr
  | Cdr of expr
[@@deriving show]
let string_of_expr = show_expr

type binding =
  | VarBinding of string * expr
  | TopLevelExpr of expr
  | TestBinding of expr
[@@deriving show]
let string_of_binding = show_binding

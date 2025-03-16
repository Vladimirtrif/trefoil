include Ast_types
open Errors

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       Int (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> Bool true
       | "false" -> Bool false
       | "nil" -> Nil
       | _ -> Var sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Node _, _ -> raise (AbstractSyntaxError ("Expression forms must start with a symbol, but got " ^ Pst.string_of_pst head))
     | Pst.Symbol "+", [left; right] -> Add (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "-", [left; right] -> Sub (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator - expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "*", [left; right] -> Mul (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator * expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "=", [left; right] -> Eq (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("operator cons expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "let", [Node [Node [Pst.Symbol x; e1]]; e2] -> Let (x, expr_of_pst e1, expr_of_pst e2)
     | Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("operator let expects 2 args with first being of the form '((x, expr))' but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "nil?", [e] -> IsNil (expr_of_pst e)
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("operator nil? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons?", [e] -> IsCons (expr_of_pst e)
     | Pst.Symbol "cons?", _ -> raise (AbstractSyntaxError ("operator cons? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "car", [e] -> Car (expr_of_pst e)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("operator car expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cdr", [e] -> Cdr (expr_of_pst e)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("operator cdr expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol f, _ -> raise (AbstractSyntaxError ("Unknown operator " ^ f))

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst


let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "test", [e] -> TestBinding (expr_of_pst e)
     | Pst.Symbol "test", _  -> raise (AbstractSyntaxError("This test is malformed " ^ Pst.string_of_pst p))
     | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()

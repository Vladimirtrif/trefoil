open Ast
open Errors

include Interpreter_types

let string_of_dynenv_entry (x, e) = x ^ " -> " ^ string_of_entry e

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name

let rec interpret_expression dynenv e =
  match e with
  | Int _ -> e
  | Bool _ -> e
  | Nil -> e
  | Var x -> begin
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound var " ^ x))
      | Some (VariableEntry value) -> value
      | Some (FunctionEntry _) -> raise (RuntimeError ("Expected variable binding but " ^ x ^ " is a function binding"))
    end
  | Add (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | Int _, v2 -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v1))
    end
  | Sub (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | Int _, v2 -> raise (RuntimeError ("Sub applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Sub applied to non-integer " ^ string_of_expr v1))
    end
  | Mul (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 * n2)
      | Int _, v2 -> raise (RuntimeError ("Mul applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Mul applied to non-integer " ^ string_of_expr v1))
    end
  | Eq (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Int _, v2 -> raise (RuntimeError ("Eq applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Eq applied to non-integer " ^ string_of_expr v1))
    end
  | Cons (e1, e2) -> let v1, v2 = interpret_expression dynenv e1, interpret_expression dynenv e2 in Cons(v1, v2)
  | If (branch, thn, els) -> if interpret_expression dynenv branch = Bool false
                              then interpret_expression dynenv els
                              else interpret_expression dynenv thn
  | Let (x, e1, e2) -> interpret_expression ((x, VariableEntry (interpret_expression dynenv e1)) :: dynenv) e2
  | IsNil e -> Bool (interpret_expression dynenv e = Nil)
  | IsCons e -> begin 
      match interpret_expression dynenv e with
      | Cons _ ->  Bool true
      | _ ->  Bool false
    end
  | Car e -> begin 
    match interpret_expression dynenv e with
      | Cons (v1, _) ->  v1
      | _ ->  raise (RuntimeError ("Car can only be applied to a cons expressions " ^ string_of_expr e))
  end
  | Cdr e -> begin 
    match interpret_expression dynenv e with
      | Cons (_, v2) ->  v2
      | _ ->  raise (RuntimeError ("Cdr can only be applied to a cons expressions " ^ string_of_expr e))
    end

let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) ->
     let value = interpret_expression dynenv e in
     Printf.printf "%s = %s\n%!" x (string_of_expr value);
     (x, VariableEntry value) :: dynenv
  | TopLevelExpr e ->
     let v = interpret_expression dynenv e in
     print_endline (string_of_expr v);
     dynenv
  | TestBinding e -> begin 
    match interpret_expression dynenv e with
    | Bool true -> dynenv
    | v -> raise (RuntimeError ("Test doesn't pass, " ^ string_of_expr e ^ " evaluates to " ^ string_of_expr v ^ " not true."))
  end

(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr

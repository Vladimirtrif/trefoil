open Ast
open Errors

type dynamic_env = (string * expr) list
let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_expr v

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
      | Some value -> value
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
      | Int _, v2 -> raise (RuntimeError ("Mul applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Mul applied to non-integer " ^ string_of_expr v1))
    end
  | Cons (e1, e2) -> let v1, v2 = interpret_expression dynenv e1, interpret_expression dynenv e2 in Cons(v1, v2)
  (* TODO: add cases for other expressions here *)
  | _ -> (failwith ("interpret_expression: not implemented " ^ string_of_expr e))

let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) ->
     let value = interpret_expression dynenv e in
     Printf.printf "%s = %s\n%!" x (string_of_expr value);
     (x, value) :: dynenv
  | TopLevelExpr e ->
     let v = interpret_expression dynenv e in
     print_endline (string_of_expr v);
     dynenv
  (* TODO: implement test bindings here *)

(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr

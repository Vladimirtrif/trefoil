open Trefoil2lib
open Errors

(* Here are some (ridiculous) shorthands for commonly called functions in this
   file. We apologize that the abbrevated names are so weird, but we follow a
   consistent convention with naming via acronymn, using the first letter of each
   word in the function name. So for example "ieab" below stands for
   "interpret_expression_after_bindings". We also use a trailing 0 to indicate
   "in the empty environment" rather than requiring an environment to be passed
   in. *)
let ie dynenv e = Interpreter.interpret_expression dynenv e
let ie0 e = ie [] e
let ib dynenv b = Interpreter.interpret_binding dynenv b
let ibs dynenv bs = Interpreter.interpret_bindings dynenv bs
let ibs0 bs = Interpreter.interpret_bindings [] bs
let eos s = Ast.expr_of_string s
let bos s = Ast.binding_of_string s
let bsos s = Ast.bindings_of_string s
let ieab dynenv bindings expr =
  Interpreter.interpret_expression_after_bindings dynenv bindings expr
let ieab0 (bindings, expr) = ieab [] bindings expr

let%test _ = Ast.Int 3 = ie0 (eos "3")
let%test _ = Ast.Int (-10) = ie0 (eos "-10")
let%test "interpret_true" = Ast.Bool true = ie0 (eos "true")

(* PARSING TESTS *)
(* here's a parsing test. *)
let%test "parsing_false" = Ast.Bool false = eos "false"

(* parsing sub tests. *)
let%test "parsing_sub" = Ast.Sub(Int 3, Int 2) = eos "(- 3 2)"
let%test "parsing_sub_error" = try ignore (eos "(- 2)"); false 
                              with AbstractSyntaxError _ -> true 

(* parsing mul tests. *)
let%test "parsing_mul" = Ast.Mul(Int 3, Int 2) = eos "(* 3 2)"
let%test "parsing_mul_error" = try ignore (eos "(* 2)"); false 
                               with AbstractSyntaxError _ -> true

(* INTERPRET TESTS *)
(* and here's an interpreter test *)
let%test "interpret_false" = Ast.Bool false = ie0 (eos "false")

(* interpreting sub tests *)
let%test "interpreting_sub1" = Ast.Int (5) = ie0 (eos "(- 10 5)")

let%test "interpreting_sub_error1" = try ignore (ie0 (eos "(- 10 false)")); false 
                                     with RuntimeError _ -> true

let%test "interpreting_sub_error2" = try ignore (ie0 (eos "(- false 5)")); false 
                                     with RuntimeError _ -> true

(* interpreting mul tests *)
let%test "interpreting_mul" = Ast.Int (20) = ie0 (eos "(* 4 5)")

let%test "interpreting_mul_error1" = try ignore (ie0 (eos "(* 10 false)")); false 
                                     with RuntimeError _ -> true

let%test "interpreting_mul_error2" = try ignore (ie0 (eos "(* false 5)")); false 
                                     with RuntimeError _ -> true

let xto3 = [("x", Ast.Int 3)]

let%test _ =
  Ast.Int 3 = ie xto3 (eos "x")

(* a test that expects a runtime error *)
let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true

let%test _ = Ast.Int 3 = ie0 (eos "(+ 1 2)")

(* a test that expects an abstract syntax error *)
let%test "test_add_abstract_syntax_error" = 
  try ignore (ie0 (eos "(+ 1)")); false
  with AbstractSyntaxError _ -> true

let%test "test_add_wrong_types" = 
  try ignore (ie0 (eos "(+ 1 true)")); false
  with RuntimeError _ -> true

let%test "interpret_sub" = Ast.Int (-1) = ie0 (eos "(- 1 2)")
let%test "interpret_mul" = Ast.Int 6 = ie0 (eos "(* 2 3)")
let%test _ = Ast.Bool true = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.Bool false = ie0 (eos "(= 4 (+ 1 2))")
let%test _ = try ignore (ie0 (eos "(= 4 true)")); false
             with RuntimeError _ -> true
let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 1)")
let%test _ = Ast.Int 1 = ie0 (eos "(if false 0 1)")
let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 x)")
let%test _ = Ast.Int 0 = ie0 (eos "(if 5 0 1)")

(* Here is a template for a parsing test for let expressions. *)
let%test _ =
  let parsed_let = eos "(let ((x 3)) (+ x 1))" in

  (* TODO: replace "Ast.Nil" on the next line with the correct AST for the
     expression above by calling your Let constructor. *)
  let manually_constructed_let = Ast.Nil in
  parsed_let = manually_constructed_let

(* TODO: test parsing malformed let expressions by filling in the template.*)
let%test _ = try ignore (eos "TODO: your expression here"); false
             with AbstractSyntaxError _ -> true

let%test "test let1" = Ast.Int 4 = ie0 (eos "(let ((x 3)) (+ x 1))")
let%test "test let2" = Ast.Int 2 = ie0 (eos "(let ((x 1)) (let ((x 2)) x))")
let%test "test let3" = Ast.Int 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")
let%test _ = Ast.Int 3 = ie0 (eos "(+ ; asdf asdf asdf \n1 2)")
let%test _ = Ast.Nil = ie0 (eos "nil")
let%test _ = Ast.Cons (Ast.Int 1, Ast.Int 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.Int 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.Int 2 = ie0 (eos "(cdr (cons 1 2))")

let%test _ = Ast.Int 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "test binding parsing" =
  let parsed_test = bos "TODO: your test binding string here" in

  (* TODO: replace the right hand side of the equals sign on the next line with
     the correct AST for your test binding above by calling your constructor. *)
  let manually_constructed_test = Ast.VarBinding("replace", Ast.Var "me") in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try ignore (bos "TODO: your malformed test binding here"); false
  with AbstractSyntaxError _ -> true

(* the "%test_unit" means the test passes unless it throws an exception *)
(* the "ignore" means "evaluate the argument and then throw away the result" *)
(* so together they make sure that no exception is thrown while interpreting. *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  ignore (ibs0 (bsos program))

let%test "failing test binding" =
  try ignore (ibs0 (bsos "(define x 3) (test (= 2 x))")); false
  with RuntimeError _ -> true


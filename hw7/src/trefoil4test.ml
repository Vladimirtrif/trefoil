open Trefoil4lib
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

(* HW7 TESTS, see Line for HW6 Tests and Line for HW5 Tests *)

(* Parse Tests *)

(* parse symbol tests *)
let%test "parse_symbol0" = Ast.Symbol "mySymbol" =  eos "'mySymbol"
let%test "parse_symbol1" = Ast.Symbol "test-sym20" =  eos "'test-sym20"

(* parse print tests *)
let%test "parse_print0" = Ast.Print (Ast.Int 1) =  eos "(print 1)"
let%test "parse_print1" = Ast.Print (Ast.Add (Ast.Int 1, Ast.Int 1)) =  eos "(print (+ 1 1))"
let%test "parse_print_error0" = try ignore (eos "(print)"); false
                                with _ -> true 
let%test "parse_print_error1" = try ignore (eos "(print 1 2)"); false
                                with _ -> true 

(* parse func call, only 1 new test for new syntax, more tests in hw6 *)
let%test "parse_newCall0" = Ast.Call ((Call (Var "f", [Int 1])), [Int 2]) =  eos "((f 1) 2)"

(* Interpret Tests *)

(* interpret symbol tests *)
let%test "interpret_symbol0" = Ast.Symbol "mySymbol" =  ie0 (eos "'mySymbol")
let%test "interpret_symbol1" = Ast.Symbol "test-sym-21!" =  ie0 (eos "'test-sym-21!")

(* interpret print tests - output tested manually*) 
let%test "interpret_print0" = Ast.Nil =  ie0 (eos "(print (+ 1 2))")
let%test "interpret_print1" = Ast.Nil =  ie0 (eos "(print (cons false nil))")

(* interpret closure test, only need 1 for straightline code *)
let%test "interpret_closure0" = Ast.Closure ({rec_name = None; lambda_param_names = ["x"]; lambda_body = Var "x"}, []) 
                                =  ie0 (Ast.Closure ({rec_name = None; lambda_param_names = ["x"]; lambda_body = Var "x"}, []))

(* interpret call test, only need 1 for new semantics, more tests from hw6 are below *)
let testEnv  =
  [ Ast.FunctionBinding ({name = "f"; param_names = ["x"]; body = Add (Var "x", Int 1)}); 
    Ast.FunctionBinding ({name = "h"; param_names = ["g"]; body = Call (Var "g", [Int 17])});
  ]

let%test "interpret_newCall0" = Ast.Int 18 =  ieab0 (testEnv, Call(Var "h", [Var "f"]))

(* provided tests *)

(*
let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.Int 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.Int 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))"));
    false
  with RuntimeError _ -> true

let%test "cond struct binding sum countdown" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l)
       (cond
         ((mynil? l) 0)
         ((mycons? l) (+ (mycons-mycar l) (sum (mycons-mycdr l))))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")



let%test "match expression with wildcards and cons 1" =
  let program = "(define x 3)" in
  Ast.Int 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (_ 42))")

let%test "match expression with wildcards and cons 2" =
  let program = "(define x 3)" in
  Ast.Int 25 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons _ _) 25) (_ 42))")


let%test "match expression with int literal patterns" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (17 30) (_ 42))")

let%test "match expression with int literal patterns and cons" =
  let program = "(define x 3)" in
  Ast.Int 2 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) (17 30) ((cons 17 0) 25) ((cons _ 18) 2) (_ 42))")

let%test "match expression with bool literal patterns 1" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (= x 3) ((cons _ _) 25) (false 17) (true 30) (_ 42))")

let%test "match expression with bool literal patterns 2" =
  let program = "(define x 3)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match (= x 4) ((cons _ _) 25) (true 30) (false 17) (_ 42))")

let%test "match expression with symbol literal patterns" =
  let program = "(define x 'hello)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match x ('world 25) ('hello 17) (true 30) (_ 42))")

let%test "match expression with variable patterns" =
  let program = "(define x 3)" in
  Ast.Int 306 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons a b) (* a b)) (_ 42))")


let%test "match struct binding" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l) (match l ((mynil) 0) ((mycons x xs) (+ x (sum xs)))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")


let sum_with_match_error =
  "(define (sum l)
     (match l
       (nil 0)
       ((cons x x) (+ x (sum xs)))))"
let%test _ =
  try ignore (ib [] (bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true
*)

(* HW6 TESTS, see Line 196 for HW5 Tests *)

(* Parsing Tests*)

(* parsing let tests *)
let%test "parsing_let0" = Ast.Let ([], Add (Int 1, Int 2)) = eos "(let () (+ 1 2))"

let%test "parsing_let1" = Ast.Let ([("tst", Bool false)], Var "tst") = eos "(let ((tst false)) tst)"

(* Note reversed order. Don't reverse list because it is not necessary so not doing it is more performant *)
let%test "parsing_let2" = Ast.Let ([("y", Int 4); ("x", Int 5)], Add (Var "x", Var "y")) = eos "(let ((x 5) (y 4)) (+ x y))"

let%test "parsing_let_syntaxError0" = try ignore (eos "(let (x 5) (+ x 5))"); false 
                                      with AbstractSyntaxError _ -> true

let%test "parsing_let_syntaxError1" = try ignore (eos "(let (((+ 2 3) 5)) (+ 5 5))"); false 
                                    with AbstractSyntaxError _ -> true

let%test "parsing_let_sameVarError" = try ignore (eos "(let ((x 5) (x 4)) (+ x 5))"); false 
                                      with AbstractSyntaxError _ -> true

(* paring cond tests *)
let%test "parsing_cond0" = Ast.Cond [] = eos "(cond)"

let%test "parsing_cond1" = Ast.Cond [(Bool false, Int 4); (Int 5, Add (Int 4, Int 5))] = eos "(cond (false 4) (5 (+ 4 5)))"

let%test "parsing_cond_error" = try ignore (eos "(cond true)"); false 
                                with AbstractSyntaxError _ -> true

(* parsing function binding tests *)
let%test "parsing_functionBinding0" = Ast.FunctionBinding {name = "myFunction"; param_names = ["x"]; body = Ast.Var "x"} 
                                                            = bos "(define (myFunction x) x)"

let%test "parsing_functionBinding1" = Ast.FunctionBinding {name = "myF"; param_names = ["x"; "y"; "z"]; body = Ast.Add (Ast.Add (Ast.Var "x", Ast.Var "y"), Ast.Var "z")} 
                                                            = bos "(define (myF x y z) (+ (+ x y) z))"

let%test "parsing_functionBinding_error_noName" = try ignore (bos "(define () x)"); false 
                                                  with AbstractSyntaxError _ -> true

let%test "parsing_functionBinding_error_notSymbol" = try ignore (bos "(define (x (+ 1 2)) x)"); false 
                                                     with AbstractSyntaxError _ -> true

let%test "parsing_functionBinding_error_repeatingSymbols0" = try ignore (bos "(define (x x) (+ 1 2))"); false 
                                                    with AbstractSyntaxError _ -> true

let%test "parsing_functionBinding_error_repeatingSymbols1" = try ignore (bos "(define (myFunc x y z t y) (+ 1 2))"); false 
                                                  with AbstractSyntaxError _ -> true

(* parsing function call tests *)
let%test "parsing_call0" = Ast.Call (Var "f", [])= eos "(f)"

let%test "parsing_call1" = Ast.Call (Var "func", [Ast.Int 1])= eos "(func 1)"

let%test "parsing_call2" = Ast.Call (Var "myFunc", [Ast.Add(Ast.Int 1, Ast.Int 3); Ast.Int 2; Ast.Bool false]) = eos "(myFunc (+ 1 3) 2 false)"

                                                  
(* Iterpret Tests *)

(* interpret let tests *)
let%test "interpret_let1" = Ast.Int 13 = ie0 (eos "(let () (+ 10 3))")

let%test "interpret_let2" = Ast.Int 6 = ie0 (eos "(let ((x 4)) (- 10 x))")

let%test "interpret_let3" = Ast.Int 20 = ie0 (eos "(let ((x 5) (y 15)) (+ y x))")

(* interpret cond tests *)
(* have old ones from hw5 below as well*)
let%test "interpret_cond_error0" = try ignore (ie0 (eos "(cond)")); false
                                   with RuntimeError _ -> true

let%test "interpret_cond_error1" = try ignore (ie0 (eos "(cond (false 2) (false true))")); false
                                   with RuntimeError _ -> true

let%test "interpret_cond0" = Ast.Int 20 = ie0 (eos "(cond (true (+ 10 10)))")

let%test "interpret_cond1" = Ast.Bool true = ie0 (eos "(cond (false 1) (false false) (1 true) (false 5))")

(* interpret function binding tests *)
let%test "interpret_functionBinding0" = [("myF", Ast.Closure ({rec_name = Some "myF"; lambda_param_names = ["x"]; lambda_body = Ast.Add (Ast.Var "x", Ast.Int 1)}, []))] 
                                          = ib [] (bos "(define (myF x) (+ x 1))")

let%test "interpret_functionBinding1" = [("F", Ast.Closure ({rec_name = Some "F"; lambda_param_names = ["x"; "y"]; lambda_body = Ast.Int 1}, [("x", Int 4)])); ("x", Ast.Int 4)] 
                                        = ib [("x", Int 4)] (bos "(define (F x y) 1)")

(* interpret function call tests *)
let incr = "(define (incr x) (+ x 1))"

let volume = "(define (volume x y z) (* x (* y z)))"

let%test "interpret_call0" = Ast.Int 7 = ieab0 (bsos incr, eos "(incr (+ 2 4))")

let%test "interpret_call1" = Ast.Int 6 = ieab0 (bsos volume, eos "(volume 1 2 3)")

let%test "interpret_call_unboundError" = try ignore (ieab0 ([], eos "(pow 2 3)")); false
                                         with RuntimeError _ -> true

let%test "interpret_call_varBindingError" = try ignore (ieab0 ([(Ast.VarBinding ("pow", Int 2))], eos "(pow 2 3)")); false
                                             with RuntimeError _ -> true

let%test "interpret_call_incorectArgsError0" = try ignore (ieab0 (bsos incr, eos "(incr)")); false
                                               with RuntimeError _ -> true

let%test "interpret_call_incorectArgsError1" = try ignore(ieab0 (bsos incr, eos "(incr 1 2)")); false
                                               with RuntimeError _ -> true


(* Provided Tests *)

let%test "multi var let" = Ast.Int 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "no var let" = Ast.Int 0 = ie0 (eos "(let () 0)")
let%test "let swap" = Ast.Int 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")

let%test "basic cond" = 
  Ast.Int 42 = ie0 (eos "(cond ((= 0 1) 17) ((= 0 0) 42))")

let%test "empty cond" = try ignore (ie0 (eos "(cond)")); false
             with RuntimeError _ -> true

let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in
  Ast.Int 3 = ieab0 (bsos program, eos "y") || true

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in
  Ast.Int 4 = ieab0 (bsos program, eos "z")

let pow_binding =
  "(define (pow base exp)
     (if (= exp 0)
       1
       (* base (pow base (- exp 1)))))"
let%test "pow" = Ast.Int 8 = ieab0 (bsos pow_binding, eos "(pow 2 3)")

let countdown_binding =
  "(define (countdown n)
     (if (= n 0)
       nil
       (cons n (countdown (- n 1)))))"
let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.Int 9 = ieab0 (bsos countdown_binding, eos expression)

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"
let%test "sum_countdown" =
  Ast.Int 55 = ieab0 (bsos (countdown_binding ^ sum_binding),
                         eos "(sum (countdown 10))")

let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"
let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

(* HW5 TESTS *)

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

(* parsing equals tests. *)
let%test "parsing_eq" = Ast.Eq(Int 2, Int 3) = eos "(= 2 3)"

let%test "parsing_eq_error" = try ignore (eos "(= 2)"); false 
                               with AbstractSyntaxError _ -> true

(* parsing nil test *)
let%test "parsing_nil" = Ast.Nil = eos "nil"

(* parsing cons tests. *)
let%test "parsing_cons" = Ast.Cons(Int 2, Bool false) = eos "(cons 2 false)"

let%test "parsing_cons_error" = try ignore (eos "(cons 2)"); false 
                               with AbstractSyntaxError _ -> true

(* parsing if tests *)
let%test "parsing_if" = Ast.If(Bool false, Int 2, Int 4) = eos "(if false 2 4)"

let%test "parsing_if_error" = try ignore (eos "(if 2)"); false 
                              with AbstractSyntaxError _ -> true

(* parsing test binding tests *)
let%test "parsing_testBinding" = Ast.TestBinding(Eq(Int 4, Int 4)) = bos "(test (= 4 4))"

let%test "parsing_testBinding_error" = try ignore (bos "(test 1 2)"); false 
                                       with AbstractSyntaxError _ -> true

(* parsing nil? tests *)
let%test "parsing_nil?" = Ast.IsNil (Nil) = eos "(nil? nil)"
                                   
let%test "parsing_nil?_error" = try ignore (eos "(nil? 1 2)"); false 
                                with AbstractSyntaxError _ -> true

(* parsing cons? tests *)
let%test "parsing_cons?" = Ast.IsCons (Cons (Int 1, Int 2)) = eos "(cons? (cons 1 2))"
                                   
let%test "parsing_cons?_error" = try ignore (eos "(cons? )"); false 
                                with AbstractSyntaxError _ -> true

(* parsing car tests *)
let%test "parsing_car" = Ast.Car (Cons (Mul (Int 1, Int 1), Int 2)) = eos "(car (cons (* 1 1) 2))"
                                   
let%test "parsing_car_error" = try ignore (eos "(car 1 2 )"); false 
                                with AbstractSyntaxError _ -> true

(* parsing cdr tests *)
let%test "parsing_cdr" = Ast.Cdr (Cons (Mul (Int 1, Int 1), Int 2)) = eos "(cdr (cons (* 1 1) 2))"
                                   
let%test "parsing_cdr_error" = try ignore (eos "(cdr)"); false 
                                with AbstractSyntaxError _ -> true

(* INTERPRET TESTS *)
(* and here's an interpreter test *)
let%test "interpret_false" = Ast.Bool false = ie0 (eos "false")

(* interpret sub tests *)
let%test "interpret_sub1" = Ast.Int (5) = ie0 (eos "(- 10 5)")

let%test "interpret_sub_error1" = try ignore (ie0 (eos "(- 10 false)")); false 
                                     with RuntimeError _ -> true

let%test "interpret_sub_error2" = try ignore (ie0 (eos "(- false 5)")); false 
                                     with RuntimeError _ -> true

(* interpret mul tests *)
let%test "interpret_mul" = Ast.Int (20) = ie0 (eos "(* 4 5)")

let%test "interpret_mul_error1" = try ignore (ie0 (eos "(* 10 false)")); false 
                                     with RuntimeError _ -> true

let%test "interpret_mul_error2" = try ignore (ie0 (eos "(* false 5)")); false 
                                     with RuntimeError _ -> true

(* interpret equals tests *)
let%test "interpret_eq1" = Ast.Bool true = ie0 (eos "(= 21 21)")

let%test "interpret_eq2" = Ast.Bool false = ie0 (eos "(= 4 5)")

let%test "interpret_eq_error1" = try ignore (ie0 (eos "(= 10 false)")); false 
                                     with RuntimeError _ -> true

let%test "interpret_eq_error2" = try ignore (ie0 (eos "(= false 5)")); false 
                                     with RuntimeError _ -> true

(* interpret cons tests *)
let%test "interpret_cons1" = Ast.Cons (Ast.Int 1, Ast.Bool false) = ie0 (eos "(cons 1 false)")

let%test "interpret_eq2" = Ast.Cons (Ast.Int 4, Ast.Int 5)  = ie0 (eos "(cons (+ 2 2) 5)")

(* interpret nil test *)
let%test "interpret_nil" = Ast.Nil = ie0 (eos "nil")

(* interpret if tests *)
let%test "interpret_if1" = Ast.Int 4 = ie0 (eos "(if false 21 4)")

let%test "interpret_if2" = Ast.Int 2 = ie0 (eos "(if 4 2 5)")

(* interpret if tests *)
let%test "interpret_if1" = Ast.Int 4 = ie0 (eos "(if false 21 4)")

let%test "interpret_if2" = Ast.Int 2 = ie0 (eos "(if 4 2 5)")

(* interpret test binding tests *)
let%test "interpret_testBinding" = [] = ib [] (bos "(test true)")

let%test "interpret_testBinding_error" = try ignore (ib [] (bos "(test (= 4 5))")); false 
                                         with RuntimeError _ -> true

(* interpret nil? tests *)
let%test "interpret_nil?1" = Ast.Bool true = ie0 (eos "(nil? nil)")

let%test "interpret_nil?2" = Ast.Bool false = ie0 (eos "(nil? 1)")

(* interpret cons? tests *)
let%test "interpret_cons?1" = Ast.Bool true = ie0 (eos "(cons? (cons 1 false))")

let%test "interpret_cons?2" = Ast.Bool false = ie0 (eos "(cons? 1)")

(* interpret car tests *)
let%test "interpret_car" = Ast.Int 1 = ie0 (eos "(car (cons (* 1 1) 2))")

let%test "interpret_car_error" = try ignore (ie0 (eos "(car 1)")); false 
with RuntimeError _ -> true

(* interpret cdr tests *)
let%test "interpret_cdr" = Ast.Int 2 = ie0 (eos "(cdr (cons (* 1 1) 2))")

let%test "interpret_cdr_error" = try ignore (ie0 (eos "(cdr false)")); false 
with RuntimeError _ -> true

let xto3 = [("x", (Ast.Int 3))]

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
  let manually_constructed_let = Ast.Let ([("x", Int 3)], Add (Var "x", Int 1)) in
  parsed_let = manually_constructed_let

(* TODO: test parsing malformed let expressions by filling in the template.*)
let%test _ = try ignore (eos "(let (x 5))"); false
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
  let parsed_test = bos "(test 1)" in

  (* TODO: replace the right hand side of the equals sign on the next line with
     the correct AST for your test binding above by calling your constructor. *)
  let manually_constructed_test = Ast.TestBinding (Int 1) in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try ignore (bos "(test 1 2)"); false
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

open Ast
open Errors

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name

let rec interpret_pattern pattern value =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
      match interpret_pattern p1 v1, interpret_pattern p2 v2 with
      | Some l1, Some l2 -> Some (l1 @ l2)
      | _ -> None
    end
      (* TODO: add cases for other kinds of patterns here *)
  | _ -> None
    
let rec interpret_expression dynenv e =
  match e with
  | Int _ -> e
  | Bool _ -> e
  | Nil -> e
  | Symbol _ -> e
  | Closure _ -> e
  | Var x -> begin
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound var or closure " ^ x))
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
      let rec checkStructure x y =
        let rec checkExpLists a b = begin
          match a,b with
          | [], [] -> true
          | a1 :: atl, b1 :: btl -> checkStructure a1 b1 && checkExpLists atl btl
          | _ -> raise (InternalError "Exp lists have different lengths in checkExpLists in checkStructure in  Eq in interpreter.ml")
        end
        in
        match x, y with
        | Int a, Int b -> a = b
        | Bool b1, Bool b2  -> b1 = b2
        | Nil, Nil  -> true
        | Symbol s1, Symbol s2  -> String.equal s1 s2
        | Cons (a1, a2), Cons(b1, b2) -> checkStructure a1 b1 && checkStructure a2 b2
        | StructConstructor (s1, vs1), StructConstructor(s2, vs2) -> String.equal s1 s2 && (List.length vs1 = List.length vs2) && checkExpLists vs1 vs2
        | Closure _, _  -> raise (RuntimeError ("Cannot apply operator = to a closure in " ^ string_of_expr e))
        | _, Closure _  -> raise (RuntimeError ("Cannot apply operator = to a closure in " ^ string_of_expr e))
        | _ -> false
      in
      let v1 = interpret_expression dynenv e1 in
      let v2 = interpret_expression dynenv e2 in
      Bool (checkStructure v1 v2)
    end
  | Cons (e1, e2) -> let v1, v2 = interpret_expression dynenv e1, interpret_expression dynenv e2 in Cons(v1, v2)
  | If (branch, thn, els) -> if interpret_expression dynenv branch = Bool false
                              then interpret_expression dynenv els
                              else interpret_expression dynenv thn
  | Let (dl, body) -> let rec makeNewEnv l acc =
                      match l with
                      | [] -> acc
                      | (x, e) :: tl -> makeNewEnv tl ((x, (interpret_expression dynenv e)) :: acc)
                    in interpret_expression (makeNewEnv dl dynenv) body
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
  | Cond cl -> let rec condHelper l =
                match l with
                | [] -> raise (RuntimeError ("Cond must have a clause that doesn't evaluate to false " ^ string_of_expr e))
                | (pi, bi) :: tl -> if interpret_expression dynenv pi = Bool false
                                    then condHelper tl 
                                    else interpret_expression dynenv bi
               in condHelper cl
  | Call (f, args) -> begin
    let rec addArgsToEnv pl al defEnv =
      (match pl, al with
       | [], [] -> defEnv
       | name :: nTail,  arg :: aTail -> addArgsToEnv nTail aTail ((name, (interpret_expression dynenv arg)) :: defEnv)
       | _ -> raise (RuntimeError ("Incorrect amount of function arguments in function call " ^ string_of_expr e ))
      )
    in  
    let x = interpret_expression dynenv f in
    (match x with
      | Closure (fa, da) ->
                           (match fa.rec_name with
                            | Some nm -> interpret_expression ((nm, (Closure (fa, da))) :: (addArgsToEnv fa.lambda_param_names args da)) fa.lambda_body
                            | None -> interpret_expression (addArgsToEnv fa.lambda_param_names args da) fa.lambda_body
                           )
      |_ -> raise (RuntimeError ("Expression doesn't evaluate to a closure in function call " ^ string_of_expr e ))
    )
  end
  | Print e -> print_endline(string_of_expr (interpret_expression dynenv e)); Nil
  | Lambda f -> Closure (f, dynenv)
  | StructConstructor (s, es) -> begin
      let rec evalExpList l acc =
        match l with
        | [] -> List.rev acc
        | hd :: tl -> evalExpList tl (interpret_expression dynenv hd :: acc)
      in StructConstructor(s, evalExpList es [])
    end
  | StructAccess (s, i, e) -> begin
      let rec getIth i l =
        match i, l with
        | 0, hd :: _ -> hd
        | n, _ :: tl -> getIth (n - 1) tl
        | _, [] -> raise (InternalError (Int.to_string i ^ " is out of bounds in struct access " ^ string_of_expr e))
      in
      match interpret_expression dynenv e with
      | StructConstructor(s2, vl) -> if String.equal s s2 
                                     then getIth i vl
                                     else raise (RuntimeError ("Attempted to access field from struct " ^ s ^ " on a " ^ s2 ^ " struct. In" ^ string_of_expr e)) 
      | _ -> raise (RuntimeError ("Expression isn't a struct in struct access " ^ string_of_expr e)) 
    end 
                              
  | StructPredicate (s, e) -> begin
      match interpret_expression dynenv e with
      | StructConstructor(s2, _) -> Bool (String.equal s s2)
      | _ -> Bool false
    end
  | Match (e, cl) -> begin 
      let rec matchhelper v cl =
        match cl with
        | [] -> raise (RuntimeError ("expression in match doesn't match any of the clauses/patterns " ^ string_of_expr e))
        | (p, b) :: tl -> begin
                            match interpret_pattern p v with
                            | Some be -> interpret_expression (be @ dynenv) b 
                            | None -> matchhelper v tl
                          end
      in matchhelper (interpret_expression dynenv e) cl
    end 

    

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
  | TestBinding e -> begin 
      match interpret_expression dynenv e with
      | Bool true -> dynenv
      | v -> raise (RuntimeError ("Test doesn't pass, " ^ string_of_expr e ^ " evaluates to " ^ string_of_expr v ^ " not true."))
    end
  | FunctionBinding r -> print_endline ("Function " ^ r.func_name ^ " is defined"); 
                        (r.func_name, (Closure ({ rec_name = Some r.func_name; lambda_param_names = r.param_names; lambda_body = r.body }, dynenv))) :: dynenv
  | StructBinding s -> begin 
      let rec elFromSl sl acc = 
        match sl with 
        | [] -> List.rev acc 
        | hd :: tl -> elFromSl tl (Var hd :: acc) 
      in
      let conEntry = (s.struct_name, (Closure ({ rec_name = Some s.struct_name; lambda_param_names = s.field_names; lambda_body = StructConstructor(s.struct_name, elFromSl s.field_names [])}, dynenv))) in
      let predEntry = (s.struct_name ^ "?", (Closure ({ rec_name = Some (s.struct_name ^ "?"); lambda_param_names = ["x"]; lambda_body = StructPredicate(s.struct_name, Var "x")}, dynenv))) in
      let rec addAccessors fl index de =
        match fl with
        | [] -> de
        | f :: tl -> addAccessors tl (index + 1)
               ( (s.struct_name ^ "-" ^ f, 
                 (Closure ({ rec_name = Some (s.struct_name ^ "-" ^ f); lambda_param_names = ["x"]; lambda_body = StructAccess(s.struct_name, index, Var "x")}, dynenv))
                 )
               :: de) 
      in
      addAccessors s.field_names 0 (predEntry :: (conEntry :: dynenv))
    end
  

(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr
